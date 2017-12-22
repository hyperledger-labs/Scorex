package examples.curvepos

import akka.actor.ActorRef
import examples.curvepos.forging.Forger
import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import scorex.core.{LocalInterface, ModifierId, VersionTag}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: SimpleBlock): Unit = {}

  override protected def onFailedTransaction(tx: SimpleTransaction): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: SimpleBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: SimpleTransaction): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: SimpleBlock): Unit = {}


  override protected def onSemanticallySuccessfulModification(mod: SimpleBlock): Unit = {}

  override protected def onSemanticallyFailedModification(mod: SimpleBlock): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = forgerRef ! Forger.StartMining

  override protected def onBetterNeighbourAppeared(): Unit = forgerRef ! Forger.StopMining
}