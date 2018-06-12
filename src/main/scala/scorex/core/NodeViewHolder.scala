package scorex.core

import akka.actor.Actor
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.serialization.Serializer
import scorex.core.transaction._
import scorex.core.transaction.state.{MinimalState, TransactionValidation}
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}


/**
  * Composite local view of the node
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * @tparam TX
  * @tparam PMOD
  */
trait NodeViewHolder[TX <: Transaction, PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
  //import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.{LocallyGeneratedTransaction, LocallyGeneratedModifier}

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]


  type NodeView = (HIS, MS, VL, MP)
  /**
    * The main data structure a node software is taking care about, a node view consists
    * of four elements to be updated atomically: history (log of persistent modifiers),
    * state (result of log's modifiers application to pre-historical(genesis) state,
    * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
    */
  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView]

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  protected def genesisState: NodeView


  protected def history(): HIS = nodeView._1

  protected def minimalState(): MS = nodeView._2

  protected def vault(): VL = nodeView._3

  protected def memoryPool(): MP = nodeView._4


  /**
    * Serializers for modifiers, to be provided by a concrete instantiation
    */
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]]

  //todo: write desc
  /**
    *
    */
  val networkChunkSize: Int


  protected type MapKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): MapKey = new mutable.WrappedArray.ofByte(id)

  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  //todo: make configurable limited size
  private val modifiersCache = mutable.Map[MapKey, PMOD]()

  protected def txModify(tx: TX): Unit = {
    //todo: async validation?
    val errorOpt: Option[Throwable] = minimalState() match {
      case txValidator: TransactionValidation[TX] =>
        txValidator.validate(tx) match {
          case Success(_) => None
          case Failure(e) => Some(e)
        }
      case _ => None
    }

    errorOpt match {
      case None =>
        memoryPool().put(tx) match {
          case Success(newPool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            val newVault = vault().scanOffchain(tx)
            updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
            context.system.eventStream.publish(SuccessfulTransaction[TX](tx))

          case Failure(e) =>
            context.system.eventStream.publish(FailedTransaction[TX](tx, e))
        }

      case Some(e) =>
        context.system.eventStream.publish(FailedTransaction[TX](tx, e))
    }
  }

  /**
    * Update NodeView with new components and notify subscribers of changed components
    *
    * @param updatedHistory
    * @param updatedState
    * @param updatedVault
    * @param updatedMempool
    */
  protected def updateNodeView(updatedHistory: Option[HIS] = None,
                               updatedState: Option[MS] = None,
                               updatedVault: Option[VL] = None,
                               updatedMempool: Option[MP] = None): Unit = {
    val newNodeView = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) {
      context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    }
    if (updatedState.nonEmpty) {
      context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    }
    if (updatedVault.nonEmpty) {
      context.system.eventStream.publish(ChangedVault())
    }
    if (updatedMempool.nonEmpty) {
      context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
    }
    nodeView = newNodeView
  }

  protected def extractTransactions(mod: PMOD): Seq[TX] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[TX] => tcm.transactions
    case _ => Seq()
  }


  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  protected def updateMemPool(blocksRemoved: Seq[PMOD], blocksApplied: Seq[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs = blocksRemoved.flatMap(extractTransactions)

    val appliedTxs = blocksApplied.flatMap(extractTransactions)

    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  private def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      context.system.eventStream.publish(DownloadRequest(tid, id))
    }

  private def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: ModifierId): IndexedSeq[PMOD] = {
    val idx = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  /**

    Assume that history knows the following blocktree:

           G
          / \
         *   G
        /     \
       *       G

    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain (* means
    that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to the root +
    application of the sidechain to the state. Assume that state is finding that some modifier in the sidechain is
    incorrect:

           G
          / \
         G   G
        /     \
       B       G
      /
     *

    In this case history should be informed about the bad modifier and it should retarget state

    //todo: improve the comment below

    We assume that we apply modifiers sequentially (on a single modifier coming from the network or generated locally),
    and in case of failed application of some modifier in a progressInfo, rollback point in an alternative should be not
    earlier than a rollback point of an initial progressInfo.
   **/

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD],
                          suffixApplied: IndexedSeq[PMOD]): (HIS, Try[MS], Seq[PMOD]) = {
    requestDownloads(progressInfo)

    case class UpdateInformation(history: HIS,
                                 state: MS,
                                 failedMod: Option[PMOD],
                                 alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                                 suffix: IndexedSeq[PMOD])

    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
        @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
        val branchingPoint = VersionTag @@ progressInfo.branchPoint.get     //todo: .get
        if (!state.version.sameElements(branchingPoint)){
          state.rollbackTo(branchingPoint) -> trimChainSuffix(suffixApplied, branchingPoint)
        } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>

        val u0 = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)

        val uf = progressInfo.toApply.foldLeft(u0) {case (u, modToApply) =>
          if(u.failedMod.isEmpty) {
            u.state.applyModifier(modToApply) match {
              case Success(stateAfterApply) =>
                val newHis = history.reportModifierIsValid(modToApply)
                context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
                //updateState(newHis, stateAfterApply, newProgressInfo, suffixTrimmed :+ modToApply)
                UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
              case Failure(e) =>
                val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
                context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
                //updateState(newHis, stateToApply, newProgressInfo, suffixTrimmed)
                UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
            }
          } else u
        }

        uf.failedMod match {
          case Some(mod) =>
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val alternativeProgressInfo = uf.alternativeProgressInfo.get
            updateState(uf.history, uf.state, alternativeProgressInfo, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  //todo: update state in async way?
  protected def pmodModify(pmod: PMOD): Unit =
    if (!history().contains(pmod.id)) {
      context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))

      log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications to the state: $progressInfo")
          context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
          context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

          if (progressInfo.toApply.nonEmpty) {
            val (newHistory, newStateTry, blocksApplied) =
              updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())

            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)

                //we consider that vault always able to perform a rollback needed
                @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get
                } else vault()
                blocksApplied.foreach(newVault.scanPersistent)

                log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
                updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))


              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state", e)
                updateNodeView(updatedHistory = Some(newHistory))
                context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
            }
          } else {
            requestDownloads(progressInfo)
            updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
          }
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
      }
    } else {
      log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")
    }


  protected def compareViews: Receive = {
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(key(mid)))
      }

      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
  }

  protected def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case (tx: TX@unchecked) if tx.modifierTypeId == Transaction.ModifierTypeId =>
            txModify(tx)

          case pmod: PMOD@unchecked =>
            if (history().contains(pmod)) {
              log.warn(s"Received modifier ${pmod.encodedId} that is already in history")
            } else if (modifiersCache.contains(key(pmod.id))) {
              log.warn(s"Received modifier ${pmod.encodedId} that is already in cache")
            } else {
              modifiersCache.put(key(pmod.id), pmod)
            }
        }

        log.debug(s"Cache before(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")

        var t: Option[PMOD] = None
        do {
          t = {
            modifiersCache.find { case (_, pmod) =>
              history().applicable(pmod)
            }.map { t =>
              val res = t._2
              modifiersCache.remove(t._1)
              res
            }
          }
          t.foreach(pmodModify)
        } while (t.isDefined)

        log.debug(s"Cache after(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")
      }
  }

  protected def processLocallyGeneratedModifiers: Receive = {
    case lt: LocallyGeneratedTransaction[TX] =>
      txModify(lt.tx)

    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)
  }

  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  protected def getNodeViewChanges: Receive = {
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (vault) sender() ! ChangedVault()
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
  }

  override def receive: Receive =
      compareViews orElse
      processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}


object NodeViewHolder {

  object ReceivableMessages {

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)
    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

    // Moved from NodeViewSynchronizer as this was only received here
    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])
    case class ModifiersFromRemote(source: ConnectedPeer, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedTransaction[TX <: Transaction](tx: TX)
    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)
  }

  // fixme: No actor is expecting this ModificationApplicationStarted and DownloadRequest messages
  // fixme: Even more, ModificationApplicationStarted seems not to be sent at all
  // fixme: should we delete these messages?
  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}