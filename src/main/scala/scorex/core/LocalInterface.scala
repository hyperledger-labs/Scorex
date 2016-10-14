package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder._
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

/**
  *
  */
trait LocalInterface[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor {

  val viewHolderRef: ActorRef

  override def preStart(): Unit = {
    val events = Seq(
      NodeViewHolder.EventType.StartingPersistentModifierApplication,

      NodeViewHolder.EventType.FailedTransaction,
      NodeViewHolder.EventType.FailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.SuccessfulPersistentModifier
    )
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case stm: StartingPersistentModifierApplication[P, TX, PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case ft: FailedTransaction[P, TX] =>
      onFailedTransaction(ft.transaction)

    case fm: FailedModification[P, TX, PMOD] =>
      onFailedModification(fm.modifier)

    case st: SuccessfulTransaction[P, TX] =>
      onSuccessfulTransaction(st.transaction)

    case sm: SuccessfulModification[P, TX, PMOD] =>
      onSuccessfulModification(sm.modifier)
  }

  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onFailedTransaction(tx: TX): Unit

  protected def onFailedModification(mod: PMOD): Unit

  protected def onSuccessfulTransaction(tx: TX): Unit

  protected def onSuccessfulModification(mod: PMOD): Unit


  override def receive: Receive = viewHolderEvents
}