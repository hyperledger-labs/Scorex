package examples.hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier}


class HybridNodeViewHolder(settings: MiningSettings) extends NodeViewHolder[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridPersistentNodeViewModifier] {

  override type SI = HybridSyncInfo

  override type HIS = HybridHistory
  override type MS = HBoxStoredState
  override type VL = HWallet
  override type MP = HMemPool

  override val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(PosBlock.ModifierTypeId -> PosBlockCompanion, PowBlock.ModifierTypeId -> PowBlockCompanion)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted so kill the whole app if that happened
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (HIS, MS, VL, MP) = {
    val GenesisAccountsNum = 20
    val GenesisBalance = 100000L

    val ew = HWallet.readOrGenerate(settings, "genesis", "e", GenesisAccountsNum)
    val genesisAccount = ew.secrets.head
    val powGenesis = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1481110008516L, -4954221073250153861L,
      0, Array.fill(32)(0: Byte), Seq())
    val genesisTxs = Seq(SimpleBoxTransaction(
      IndexedSeq(genesisAccount -> 0),
      ew.publicKeys.map(_ -> GenesisBalance).toIndexedSeq,
      0L,
      0L))

    val genesisBox = PublicKey25519NoncedBox(genesisAccount.publicImage, 0, GenesisBalance)
    val posGenesis = PosBlock.create(powGenesis.id, 0, genesisTxs, genesisBox, genesisAccount)

    var history = HybridHistory.readOrGenerate(settings)
    history = history.append(powGenesis).get._1
    history = history.append(posGenesis).get._1

    val gs = HBoxStoredState.genesisState(settings, Seq(posGenesis, powGenesis))
    val gw = HWallet.genesisWallet(settings, Seq(posGenesis, powGenesis))

    gw.boxes().foreach(b => assert(gs.closedBox(b.box.id).isDefined))

    (history, gs, gw, HMemPool.emptyPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(HIS, MS, VL, MP)] = {
    if (HWallet.exists(settings)) {
      Some((
        HybridHistory.readOrGenerate(settings),
        HBoxStoredState.readOrGenerate(settings),
        HWallet.readOrGenerate(settings),
        HMemPool.emptyPool))
    } else None
  }
}