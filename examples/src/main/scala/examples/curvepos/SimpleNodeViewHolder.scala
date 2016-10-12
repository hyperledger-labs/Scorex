package examples.curvepos

import examples.curvepos.transaction._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {
  override type HIS = SimpleBlockchain

  override def restoreState(): Option[(HIS, MS, VL, MP)] = None

  //todo: ???
  override def fixDb(): Unit = ???

  override protected def genesisState: (HIS, MS, VL, MP) =
    (new SimpleBlockchain, new SimpleState, SimpleWallet(), new SimpleMemPool)

  override type MS = SimpleState
  override type VL = SimpleWallet
  override type MP = SimpleMemPool
  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = null
}
