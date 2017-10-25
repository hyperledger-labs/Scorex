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
      NodeViewHolder.EventType.StartingPersistentModifierApplication,

      NodeViewHolder.EventType.FailedTransaction,
      NodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
      NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier
    )
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case stm: StartingPersistentModifierApplication[PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case ft: FailedTransaction[P, TX] =>
      onFailedTransaction(ft.transaction)

    case fm: SyntacticallyFailedModification[PMOD] =>
      onFailedModification(fm.modifier)

    case st: SuccessfulTransaction[P, TX] =>
      onSuccessfulTransaction(st.transaction)

    case sm: SyntacticallySuccessfulModifier[PMOD] =>
      onSuccessfulModification(sm.modifier)
  }

  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onFailedTransaction(tx: TX): Unit

  protected def onFailedModification(mod: PMOD): Unit

  protected def onSuccessfulTransaction(tx: TX): Unit

  protected def onSuccessfulModification(mod: PMOD): Unit

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