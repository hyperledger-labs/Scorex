package examples.curvepos

import examples.curvepos.transaction._
import scorex.core.NodeViewHolder
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {
  override type HIS = Blockchain

  override def restoreState(): Option[(HIS, MS, WL, MP)] = ???

  //todo: ???
  override def fixDb(): Unit = ???

  override protected def genesisState: (HIS, MS, WL, MP) = ???

  override type MS = MinimalStateImpl
  override type WL = SimpleWallet
  override type MP = SimplestMemPool
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = null
}
