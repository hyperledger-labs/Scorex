package examples.curvepos

import examples.curvepos.transaction.{SimplePayment, _}
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PublicKey}

import scala.util.{Failure, Success}

class SimpleNodeViewHolder(settings: Settings)
  extends NodeViewHolder[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {


  override val networkChunkSize: Int = settings.networkChunkSize
  override type SI = SimpleSyncInfo

  override type HIS = SimpleBlockchain
  override type MS = SimpleState
  override type VL = SimpleWallet
  override type MP = SimpleMemPool

  override lazy val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(SimpleBlock.ModifierTypeId -> SimpleBlockCompanion)

  override def restoreState(): Option[(HIS, MS, VL, MP)] = None

  override protected def genesisState: (HIS, MS, VL, MP) = {
    val emptyBlockchain = new SimpleBlockchain
    val emptyState = new SimpleState

    val genesisAcc1 = SimpleWallet(Base58.decode("genesis").get).publicKeys.head
    val genesisAcc2 = SimpleWallet(Base58.decode("genesis2").get).publicKeys.head

    val IntitialBaseTarget = 15372286700L
    val generator = PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(0: Byte))
    val toInclude: Seq[SimpleTransaction] = Seq(
      SimplePayment(genesisAcc1, genesisAcc1, 50000000, 0, 1, 0),
      SimplePayment(genesisAcc2, genesisAcc2, 50000000, 0, 1, 0)
    )

    val genesisBlock: SimpleBlock = SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
      0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), IntitialBaseTarget, generator, toInclude)

    val blockchain = emptyBlockchain.append(genesisBlock) match {
      case Failure(f) => throw f
      case Success(newBlockchain) => newBlockchain._1
    }
    require(blockchain.height() == 1, s"${blockchain.height()} == 1")

    val state = emptyState.applyModifier(genesisBlock) match {
      case Failure(f) => throw f
      case Success(newState) => newState
    }
    require(!state.isEmpty)

    log.info(s"Genesis state with block (id: ${genesisBlock.id}) ${genesisBlock.json.noSpaces} created")

    (blockchain, state, SimpleWallet(settings.walletSeed), new SimpleMemPool)
  }
}
