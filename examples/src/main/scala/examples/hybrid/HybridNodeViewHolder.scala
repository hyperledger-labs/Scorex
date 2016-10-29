package examples.hybrid

import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{SimpleBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition


class HybridNodeViewHolder extends NodeViewHolder[
  PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridPersistentNodeViewModifier] {

  override type SI = HybridSyncInfo

  override type HIS = HybridHistory
  override type MS = SimpleBoxStoredState
  override type VL = HWallet
  override type MP = HMemPool

  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = Map()

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (HIS, MS, VL, MP) = ???

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(HIS, MS, VL, MP)] = ???
}
