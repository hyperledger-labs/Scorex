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
    case StartingPersistentModifierApplication(pmod: PMOD) =>
      onStartingPersistentModifierApplication(pmod)

    case FailedTransaction(tx: TX, throwable, source) =>
      onFailedTransaction(tx)

    case FailedModification(mod: PMOD, throwable, source) =>
      onFailedModification(mod)

    case SuccessfulTransaction(tx: TX, source) =>
      onSuccessfulTransaction(tx)


    case SuccessfulModification(mod: PMOD, source) =>
      onSuccessfulModification(mod)
  }

  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onFailedTransaction(tx: TX): Unit

  protected def onFailedModification(mod: PMOD): Unit

  protected def onSuccessfulTransaction(tx: TX): Unit

  protected def onSuccessfulModification(mod: PMOD): Unit


  override def receive: Receive = viewHolderEvents
}