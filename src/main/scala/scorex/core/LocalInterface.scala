package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.NodeViewHolder._
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

/**
  *
  */
trait LocalInterface[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor with ScorexLogging {

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


  override def receive: Receive = viewHolderEvents orElse {
    case lt: LocallyGeneratedTransaction[P, TX] =>
      viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[P, TX, PMOD] =>
      viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}

object LocalInterface {

  case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)

  case class LocallyGeneratedModifier[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (pmod: PMOD)

}