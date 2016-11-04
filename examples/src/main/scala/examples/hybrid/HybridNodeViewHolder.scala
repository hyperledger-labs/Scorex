package examples.hybrid

import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

import scala.util.Random


class HybridNodeViewHolder(settings: Settings) extends NodeViewHolder[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridPersistentNodeViewModifier] {

  override type SI = HybridSyncInfo

  override type HIS = HybridHistory
  override type MS = HBoxStoredState
  override type VL = HWallet
  override type MP = HMemPool

  override val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] =
    Map(PosBlock.ModifierTypeId -> PosBlockCompanion, PowBlock.ModifierTypeId -> PowBlockCompanion)

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (HIS, MS, VL, MP) = {
    val ew = HWallet.emptyWallet(settings, "genesis", "e", 10)

    val genesisAccount = ew.secrets.head

    val genesisTxs = ew.publicKeys.map { pubkey =>
      SimpleBoxTransaction(IndexedSeq(genesisAccount -> Random.nextLong()), IndexedSeq(pubkey -> 100000), 0L, 0L)
    }.toSeq

    val za = Array.fill(32)(0: Byte)
    val initialBlock = PosBlock(za, 0, genesisTxs, ew.publicKeys.head, Signature25519(za))

    val gs = HBoxStoredState.genesisState(settings, initialBlock)
    val gw = HWallet.genesisWallet(settings, initialBlock)

    (HybridHistory.emptyHistory(settings), gs, gw, HMemPool.emptyPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  //todo: restore state from database
  override def restoreState(): Option[(HIS, MS, VL, MP)] = None
}