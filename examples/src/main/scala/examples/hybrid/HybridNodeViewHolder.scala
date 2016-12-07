package examples.hybrid

import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.mining.PowMiner
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Random


class HybridNodeViewHolder(settings: Settings) extends NodeViewHolder[PublicKey25519Proposition,
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
    val ew = HWallet.readOrGenerate(settings, "genesis", "e", 500)
    val genesisAccount = ew.secrets.head

    var history = HybridHistory.readOrGenerate(settings)

    val powGenesis = PowBlock(PowMiner.GenesisParentId, PowMiner.GenesisParentId, 1481110008516L, -4954221073250153861L, 0, Array.fill(32)(0: Byte), Seq())
    history = history.append(powGenesis).get._1

    val posGenesis = PosBlock(Base58.decode("1P27qcVfiFxScqaG5orvbz4qyERvWr63p8DHocPgJSD").get, 1481110008680L, Seq(),
      genesisAccount.publicImage, Signature25519(Array.fill(64)(0: Byte)))

    history = history.append(posGenesis).get._1

    val genesisTxs = ew.publicKeys.flatMap { pubkey =>
      (1 to 10).map(_ =>
        SimpleBoxTransaction(
          IndexedSeq(genesisAccount -> Random.nextLong()),
          IndexedSeq(pubkey -> (100L + Random.nextInt(100000))),
          0L,
          0L))
    }.toSeq

    val za = Array.fill(Curve25519.SignatureLength)(0: Byte)
    val initialBlock = PosBlock(PowMiner.GenesisParentId, 0, genesisTxs, ew.publicKeys.head, Signature25519(za))

    val gs = HBoxStoredState.genesisState(settings, initialBlock)
    val gw = HWallet.genesisWallet(settings, initialBlock)

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