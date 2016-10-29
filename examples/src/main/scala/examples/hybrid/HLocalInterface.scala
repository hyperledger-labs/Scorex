package examples.hybrid

import akka.actor.ActorRef
import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

//todo: add refs to pow miner / forger to constructor params
class HLocalInterface(override val viewHolderRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier] {

  override protected def onStartingPersistentModifierApplication(pmod: HybridPersistentNodeViewModifier): Unit = {}

  override protected def onFailedTransaction(tx: SimpleBoxTransaction): Unit = {}

  override protected def onFailedModification(mod: HybridPersistentNodeViewModifier): Unit = {}

  override protected def onSuccessfulTransaction(tx: SimpleBoxTransaction): Unit = {}

  override protected def onSuccessfulModification(mod: HybridPersistentNodeViewModifier): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}
}