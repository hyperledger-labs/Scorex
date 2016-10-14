package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder._

/**
  *
  */
class LocalInterface(viewHolderRef: ActorRef) extends Actor {

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
    case StartingPersistentModifierApplication(pmod) =>

    case FailedTransaction(tx, throwable, source) =>


    case FailedModification(mod, throwable, source) =>


    case SuccessfulTransaction(tx, source) =>


    case SuccessfulModification(mod, source) =>

  }


  override def receive: Receive = viewHolderEvents
}
