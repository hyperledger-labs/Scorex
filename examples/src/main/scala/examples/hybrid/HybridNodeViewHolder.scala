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
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58


class HybridNodeViewHolder(settings: MiningSettings) extends NodeViewHolder[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock] {
  override val networkChunkSize: Int = settings.networkChunkSize

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
    val GenesisAccountsNum = 10
    val GenesisBalance = 100000000L

    val ew = HWallet.readOrGenerate(settings, "genesis", "e", GenesisAccountsNum)
    val icoMembers = ew.publicKeys.ensuring(_.size == GenesisAccountsNum).toIndexedSeq.sortBy(_.address)
    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val genesisAccountPriv = genesisAccount._1
    val powGenesis = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1481110008516L, 38,
      0, Array.fill(32)(0: Byte), genesisAccount._2, Seq())


    val genesisTxs = Seq(SimpleBoxTransaction(
      IndexedSeq(genesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L))
    log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")
    assert(Base58.encode(genesisTxs.head.id) == "J26Fp2sChi6WPS8no7h94zJwrVv3UoqBHwNhb3bEHNgY")

    val genesisBox = PublicKey25519NoncedBox(genesisAccountPriv.publicImage, 0, GenesisBalance)
    val attachment = "genesis attachment".getBytes
    val posGenesis = PosBlock.create(powGenesis.id, 0, genesisTxs, genesisBox, attachment, genesisAccountPriv)

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