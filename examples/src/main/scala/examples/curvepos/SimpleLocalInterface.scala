package examples.curvepos

import akka.actor.ActorRef
import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: SimpleBlock): Unit = {} //TODO

  override protected def onFailedTransaction(tx: SimpleTransaction): Unit = {} //TODO

  override protected def onFailedModification(mod: SimpleBlock): Unit = {} //TODO

  override protected def onSuccessfulTransaction(tx: SimpleTransaction): Unit = {} //TODO

  override protected def onSuccessfulModification(mod: SimpleBlock): Unit = {} //TODO
}
