package scorex.core

import akka.actor.Actor
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.serialization.Serializer
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
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
  * @tparam P
  * @tparam TX
  * @tparam PMOD
  */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  import NodeViewHolder._
  import NodeViewHolder.ReceivableMessages._
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{RequestFromLocal, ChangedHistory,
                                                                      ChangedMempool, ChangedVault,
                                                                      SuccessfulTransaction, FailedTransaction,
                                                                      SyntacticallySuccessfulModifier, SyntacticallyFailedModification,
                                                                      SemanticallySuccessfulModifier, SemanticallyFailedModification}
  import scorex.core.LocalInterface.ReceivableMessages.{ChangedState, RollbackFailed, NewOpenSurface, StartingPersistentModifierApplication}
  import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.{LocallyGeneratedTransaction, LocallyGeneratedModifier}

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[P, TX, PMOD, VL]
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
      case txValidator: TransactionValidation[P, TX] =>
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
            context.system.eventStream.publish(SuccessfulTransaction[P, TX](tx))

          case Failure(e) =>
            context.system.eventStream.publish(FailedTransaction[P, TX](tx, e))
        }

      case Some(e) =>
        context.system.eventStream.publish(FailedTransaction[P, TX](tx, e))
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
    case tcm: TransactionsCarryingPersistentNodeViewModifier[P, TX] => tcm.transactions
    case _ => Seq()
  }


  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  protected def updateMemPool(progressInfo: History.ProgressInfo[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs = progressInfo.toRemove.flatMap(extractTransactions)

    val appliedTxs = progressInfo.toApply.map(extractTransactions).getOrElse(Seq())

    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[P, TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  private def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      context.system.eventStream.publish(DownloadRequest(tid, id))
    }

  /*

    Assume history knew following blocktree:

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

    todo: write tests for this case
       */

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD]): (HIS, Try[MS]) = {
    requestDownloads(progressInfo)

    val stateToApplyTry: Try[MS] = if (progressInfo.chainSwitchingNeeded) {
      Try {
        val branchingPoint = VersionTag @@ progressInfo.branchPoint.get

        if (!state.version.sameElements(branchingPoint)) {
          state.rollbackTo(branchingPoint).map { rs =>
            rs
          }
        } else Success(state)
      }.flatten
    } else Success(state)

    stateToApplyTry match {
      case Success(stateToApply) =>
        progressInfo.toApply match {
          case Some(modToApply) =>
            stateToApply.applyModifier(modToApply) match {
              case Success(stateAfterApply) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = true, modToApply.id)
                context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
                updateState(newHis, stateAfterApply, newProgressInfo)
              case Failure(e) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = false, ModifierId @@ state.version)
                context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
                updateState(newHis, stateToApply, newProgressInfo)
            }

          case None =>
            history -> Success(stateToApply)
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
            val (newHistory, newStateTry) = updateState(historyBeforeStUpdate, minimalState(), progressInfo)
            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo, memoryPool(), newMinState)

                //we consider that vault always able to perform a rollback needed
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get.scanPersistent(progressInfo.toApply)
                } else {
                  vault().scanPersistent(progressInfo.toApply)
                }

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
            if (history().contains(pmod) || modifiersCache.contains(key(pmod.id))) {
              log.warn(s"Received modifier ${pmod.encodedId} that is already in history")
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
    case lt: LocallyGeneratedTransaction[P, TX] =>
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

  }

//  object EventType extends Enumeration {
//    //finished modifier application, successful of failed
//    val FailedTransaction: EventType.Value = Value(1)
//    val SyntacticallyFailedPersistentModifier: EventType.Value = Value(2)
//    val SemanticallyFailedPersistentModifier: EventType.Value = Value(3)
//    val SuccessfulTransaction: EventType.Value = Value(4)
//    val SuccessfulSyntacticallyValidModifier: EventType.Value = Value(5)
//    val SuccessfulSemanticallyValidModifier: EventType.Value = Value(6)
//
//
//    //starting persistent modifier application. The application could be slow
//    val StartingPersistentModifierApplication: EventType.Value = Value(7)
//
//    val OpenSurfaceChanged: EventType.Value = Value(8)
//
//    //rollback failed, really wrong situation, probably
//    val FailedRollback: EventType.Value = Value(9)
//
//    val DownloadNeeded: EventType.Value = Value(10)
//
//    val StateChanged: EventType.Value = Value(11)
//    val HistoryChanged: EventType.Value = Value(12)
//    val MempoolChanged: EventType.Value = Value(13)
//    val VaultChanged: EventType.Value = Value(14)
//  }

  // fixme: No actor is expecting this ModificationApplicationStarted and DownloadRequest messages
  // fixme: Even more, ModificationApplicationStarted seems not to be sent at all
  // fixme: should we delete these messages?
  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}