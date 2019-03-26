package scorex.core

import akka.actor.Actor
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.settings.ScorexSettings
import scorex.core.transaction._
import scorex.core.transaction.state.{MinimalState, TransactionValidation}
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexEncoding
import scorex.util.ScorexLogging
import scala.annotation.tailrec
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
  extends Actor with ScorexLogging with ScorexEncoding {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]

  type NodeView = (HIS, MS, VL, MP)

  case class UpdateInformation(history: HIS,
                               state: MS,
                               failedMod: Option[PMOD],
                               alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                               suffix: IndexedSeq[PMOD])

  val scorexSettings: ScorexSettings

  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  protected lazy val modifiersCache: ModifiersCache[PMOD, HIS] =
    new DefaultModifiersCache[PMOD, HIS](scorexSettings.network.maxModifiersCacheSize)

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
            context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
        }

      case Some(e) =>
        context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
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
      context.system.eventStream.publish(ChangedVault(newNodeView._3.getReader))
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
      !appliedTxs.exists(t => t.id == tx.id) && {
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

  private def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: scorex.util.ModifierId): IndexedSeq[PMOD] = {
    val idx = suffix.indexWhere(_.id == rollbackPoint)
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

    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
        @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
        val branchingPoint = progressInfo.branchPoint.get //todo: .get
        if (state.version != branchingPoint) {
          state.rollbackTo(idToVersion(branchingPoint)) -> trimChainSuffix(suffixApplied, branchingPoint)
        } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        val stateUpdateInfo = applyState(history, stateToApply, suffixTrimmed, progressInfo)

        stateUpdateInfo.failedMod match {
          case Some(_) =>
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val alternativeProgressInfo = stateUpdateInfo.alternativeProgressInfo.get
            updateState(stateUpdateInfo.history, stateUpdateInfo.state, alternativeProgressInfo, stateUpdateInfo.suffix)
          case None => (stateUpdateInfo.history, Success(stateUpdateInfo.state), stateUpdateInfo.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  protected def applyState(history: HIS,
                           stateToApply: MS,
                           suffixTrimmed: IndexedSeq[PMOD],
                           progressInfo: ProgressInfo[PMOD]): UpdateInformation = {
    val updateInfoSample = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
    progressInfo.toApply.foldLeft(updateInfoSample) { case (updateInfo, modToApply) =>
      if (updateInfo.failedMod.isEmpty) {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHis = history.reportModifierIsValid(modToApply)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
            UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)
          case Failure(e) =>
            val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      } else updateInfo
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
                  vault().rollback(idToVersion(progressInfo.branchPoint.get)).get
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

  /**
    * Process new modifiers from remote.
    * Put all candidates to modifiersCache and then try to apply as much modifiers from cache as possible.
    * Clear cache if it's size exceeds size limit.
    * Publish `ModifiersProcessingResult` message with all just applied and removed from cache modifiers.
    */
  protected def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(mods: Seq[PMOD]) =>
      mods.foreach(m => modifiersCache.put(m.id, m))

      log.debug(s"Cache size before: ${modifiersCache.size}")

      @tailrec
      def applyLoop(applied: Seq[PMOD]): Seq[PMOD] = {
        modifiersCache.popCandidate(history()) match {
          case Some(mod) =>
            pmodModify(mod)
            applyLoop(mod +: applied)
          case None =>
            applied
        }
      }

      val applied = applyLoop(Seq())
      val cleared = modifiersCache.cleanOverfull()

      context.system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
      log.debug(s"Cache size after: ${modifiersCache.size}")
  }

  protected def transactionsProcessing: Receive = {
    case newTxs: NewTransactions[TX] =>
      newTxs.txs.foreach(txModify)
    case EliminateTransactions(ids) =>
      val updatedPool = memoryPool().filter(tx => !ids.contains(tx.id))
      updateNodeView(updatedMempool = Some(updatedPool))
      ids.foreach { id =>
        val e = new Exception("Became invalid")
        context.system.eventStream.publish(FailedTransaction(id, e, immediateFailure = false))
      }
  }

  protected def processLocallyGeneratedModifiers: Receive = {
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
      if (vault) sender() ! ChangedVault(nodeView._3.getReader)
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
  }

  override def receive: Receive =
    processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      transactionsProcessing orElse
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

    // Modifiers received from the remote peer with new elements in it
    case class ModifiersFromRemote[PM <: PersistentNodeViewModifier](modifiers: Iterable[PM])

    sealed trait NewTransactions[TX <: Transaction]{
      val txs: Iterable[TX]
    }

    case class LocallyGeneratedTransaction[TX <: Transaction](tx: TX) extends NewTransactions[TX] {
      override val txs: Iterable[TX] = Iterable(tx)
    }

    case class TransactionsFromRemote[TX <: Transaction](txs: Iterable[TX]) extends NewTransactions[TX]

    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

    case class EliminateTransactions(ids: Seq[scorex.util.ModifierId])

  }

  // fixme: No actor is expecting this ModificationApplicationStarted and DownloadRequest messages
  // fixme: Even more, ModificationApplicationStarted seems not to be sent at all
  // fixme: should we delete these messages?
  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: scorex.util.ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}
