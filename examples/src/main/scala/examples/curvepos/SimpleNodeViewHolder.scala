package examples.curvepos

import examples.curvepos.transaction.{SimpleWallet, MinimalStateImpl, SimpleNodeViewModifier, FeeTransaction}
import scorex.core.NodeViewHolder
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, FeeTransaction, SimpleNodeViewModifier] {
  override type HIS = this.type

  override def restoreState(): Option[(HIS, SimpleNodeViewHolder.this.type, SimpleNodeViewHolder.this.type, SimpleNodeViewHolder.this.type)] = ???

  //todo: ???
  override def fixDb(): Unit = ???

  override protected def genesisState: (HIS, SimpleNodeViewHolder.this.type, SimpleNodeViewHolder.this.type, SimpleNodeViewHolder.this.type) = ???

  override type MS = MinimalStateImpl
  override type WL = SimpleWallet
  override type MP = this.type
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = _
}
