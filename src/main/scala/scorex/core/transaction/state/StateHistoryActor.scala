package scorex.core.transaction.state

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.NodeViewComponent.{HistoryComponent, StateComponent}
import scorex.core.NodeViewComponentOperation.{GetReader, OperationMode}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.SyncInfo
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.transaction.Transaction
import scorex.core.transaction.state.StateOperation._
import scorex.core.utils.ScorexEncoding
import scorex.core.{PersistentNodeViewModifier, consensus, idToVersion}
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class StateHistoryActor[TX <: Transaction,
    PMOD <: PersistentNodeViewModifier,
    SI <: SyncInfo,
    State <: MinimalState[PMOD, State],
    History <: consensus.History[PMOD, SI, History]
](private var state: State, private var history: History)
  extends Actor with ScorexLogging with ScorexEncoding {

  def receive: Receive = applyModifier orElse getReader orElse validate

  val applyModifier: Receive = {
    case ApplyModifier(modifier, mode) =>
      val result = applyPersistentModifier(modifier.asInstanceOf[PMOD], mode)
      sender ! result
  }

  val getReader: Receive = {
    case GetReader(StateComponent) =>
      val reader = state.getReader
      sender ! reader
    case GetReader(HistoryComponent) =>
      val reader = history.getReader
      sender ! reader
  }

  val validate: Receive = {
    case request: ValidateTransaction[TX] =>
      val response = txValidation(request)
      sender ! response
  }

  private def txValidation(request: ValidateTransaction[TX]): TransactionValidationResponse[TX] = {
    val ValidateTransaction(tx, mode) = request
    state match {
      case txValidator: TransactionValidation[TX] =>
        TransactionValidationResponse(tx, txValidator.validate(tx), mode)

      case _ =>
        TransactionValidationResponse(tx, Success(()), mode)
    }
  }

  protected def applyPersistentModifier(pmod: PMOD,
                                        mode: OperationMode[ApplyModifier[_]]): PersistentModifierResponse[PMOD] = {
    if (history.contains(pmod.id)) {
      log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")
      PersistentModifierResponse(Set.empty, ProgressInfo.empty, Seq.empty, pmod, mode)
    } else {
      context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))
      log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")
      history.append(pmod) match {
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
          PersistentModifierResponse(Set.empty, ProgressInfo.empty, Seq.empty, pmod, mode)
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications to the state: $progressInfo")
          context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
          context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))
          if (progressInfo.toApply.isEmpty) {
            setHistory(historyBeforeStUpdate)
            PersistentModifierResponse(Set(HistoryComponent), progressInfo, Seq.empty, pmod, mode)
          } else {
            updateState(historyBeforeStUpdate, state, progressInfo, IndexedSeq()) match {
              case Success(UpdateInformation(newHistory, newState, _, _, suffix)) =>
                log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
                setHistory(newHistory)
                setState(newState)
                PersistentModifierResponse(Set(HistoryComponent, StateComponent), progressInfo, suffix, pmod, mode)
              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) " +
                                  s"to minimal state", e)
                context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
                PersistentModifierResponse(Set.empty, progressInfo, Seq.empty, pmod, mode)
            }
          }
      }
    }
  }

  /**
  Assume that history knows the following blocktree:
    {{{
           G
          / \
      *   G
        /     \
      *       G
    }}}
    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain (* means
    that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to the root +
    application of the sidechain to the state. Assume that state is finding that some modifier in the sidechain is
    incorrect:
    {{{
           G
          / \
         G   G
        /     \
       B       G
      /
      *
    }}}
    In this case history should be informed about the bad modifier and it should retarget state

    //todo: improve the comment below

    We assume that we apply modifiers sequentially (on a single modifier coming from the network or generated locally),
    and in case of failed application of some modifier in a progressInfo, rollback point in an alternative should be not
    earlier than a rollback point of an initial progressInfo.
    **/
  @tailrec
  private def updateState(history: History,
                          state: State,
                          progressInfo: ProgressInfo[PMOD],
                          suffixApplied: IndexedSeq[PMOD]): Try[UpdateInformation] = {

    val (stateToApplyTry: Try[State], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
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
          case None => Success(stateUpdateInfo)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  protected def applyState(history: History,
                           stateToApply: State,
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

  private def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: scorex.util.ModifierId): IndexedSeq[PMOD] = {
    val idx = suffix.indexWhere(_.id == rollbackPoint)
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  private def setState(newState: State): Unit = {
    state = newState
  }

  private def setHistory(newHistory: History): Unit = {
    history = newHistory
  }

  case class UpdateInformation(history: History,
                               state: State,
                               failedMod: Option[PMOD],
                               alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                               suffix: IndexedSeq[PMOD])

}


object StateHistoryActor {

  def props[TX <: Transaction,
  PMOD <: PersistentNodeViewModifier,
  SI <: SyncInfo,
  State <: MinimalState[PMOD, State],
  History <: consensus.History[PMOD, SI, History]](state: State, history: History): Props = {
    Props(new StateHistoryActor[TX, PMOD, SI, State, History](state, history))
  }

  def apply[TX <: Transaction,
  PMOD <: PersistentNodeViewModifier,
  SI <: SyncInfo,
  State <: MinimalState[PMOD, State],
  History <: consensus.History[PMOD, SI, History]](state: State, history: History)
                                                  (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props[TX, PMOD, SI, State, History](state, history))
  }
}
