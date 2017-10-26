package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.{BetterNeighbourAppeared, LocallyGeneratedModifier, LocallyGeneratedTransaction, NoBetterNeighbour}
import scorex.core.NodeViewHolder._
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

/**
  *
  */
trait LocalInterface[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  val viewHolderRef: ActorRef

  override def preStart(): Unit = {
    val events = Seq(
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.FailedTransaction,

      NodeViewHolder.EventType.StartingPersistentModifierApplication,
      NodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SemanticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
      NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier,

      NodeViewHolder.EventType.OpenSurfaceChanged,
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.FailedRollback
    )
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case st: SuccessfulTransaction[P, TX] =>
      onSuccessfulTransaction(st.transaction)

    case ft: FailedTransaction[P, TX] =>
      onFailedTransaction(ft.transaction)


    case stm: StartingPersistentModifierApplication[PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case syns: SyntacticallySuccessfulModifier[PMOD] =>
      onSyntacticallySuccessfulModification(syns.modifier)

    case synf: SyntacticallyFailedModification[PMOD] =>
      onSyntacticallyFailedModification(synf.modifier)

    case sems: SemanticallySuccessfulModifier[PMOD] =>
      onSemanticallySuccessfulModification(sems.modifier)

    case semf: SemanticallyFailedModification[PMOD] =>
      onSemanticallyFailedModification(semf.modifier)

    case surf: NewOpenSurface =>
      onNewSurface(surf.newSurface)

    case state: ChangedState =>
      onChangedState(state.isRollback, state.newVersion)

    case RollbackFailed =>
      onRollbackFailed()
  }


  protected def onSuccessfulTransaction(tx: TX): Unit
  protected def onFailedTransaction(tx: TX): Unit


  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onSyntacticallySuccessfulModification(mod: PMOD): Unit
  protected def onSyntacticallyFailedModification(mod: PMOD): Unit

  protected def onSemanticallySuccessfulModification(mod: PMOD): Unit
  protected def onSemanticallyFailedModification(mod: PMOD): Unit

  protected def onNewSurface(newSurface: Seq[ModifierId]): Unit
  protected def onChangedState(isRollback: Boolean, newVersion: VersionTag): Unit
  protected def onRollbackFailed(): Unit


  protected def onNoBetterNeighbour(): Unit
  protected def onBetterNeighbourAppeared(): Unit

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour =>
      onNoBetterNeighbour()
    case BetterNeighbourAppeared =>
      onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[P, TX] =>
      viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[PMOD] =>
      viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}

object LocalInterface {

  case object NoBetterNeighbour

  case object BetterNeighbourAppeared

  case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)

  case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)
}