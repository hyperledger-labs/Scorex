package hybrid

import java.io.File

import commons.ExamplesCommonGenerators
import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{PosBlock, PowBlock, PowBlockCompanion, PowBlockHeader}
import examples.hybrid.history.{HistoryStorage, HybridHistory, HybridSyncInfo}
import examples.hybrid.mining.MiningSettings
import io.circe
import io.iohk.iodb.LSMStore
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.NodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{BoxStateChanges, Insertion, PrivateKey25519}
import scorex.core.transaction.wallet.WalletBox
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._
import scala.util.Random

trait HybridGenerators extends ExamplesCommonGenerators {
  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")

    override lazy val targetBlockDelay: Long = 3.seconds.toMillis

    override lazy val Difficulty: BigInt = 1
  }

  lazy val hybridSyncInfoGen: Gen[HybridSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    pos <- genBytesList(NodeViewModifier.ModifierIdSize)
    pow <- genBytesList(NodeViewModifier.ModifierIdSize)
    pows <- Gen.nonEmptyListOf(pow).map(_.take(HybridSyncInfo.MaxLastPowBlocks))
  } yield HybridSyncInfo(answer, pows, pos)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val blockIdGen: Gen[BlockId] = genBytesList(Block.BlockIdLength)

  lazy val blockIdsGen: Gen[Seq[BlockId]] = Gen.listOf(blockIdGen)

  lazy val nonEmptyBlockIdsGen: Gen[Seq[BlockId]] = Gen.nonEmptyListOf(blockIdGen)

  lazy val posBlockGen: Gen[PosBlock] = for {
    timestamp: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransaction] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionGen))
    box: PublicKey25519NoncedBox <- noncedBoxGen
    attach: Array[Byte] <- genBoundedBytes(0, 4096)
    generator: PrivateKey25519 <- key25519Gen.map(_._1)
    posParentId: Array[Byte] <- genBytesList(Block.BlockIdLength)
  } yield PosBlock.create(posParentId, timestamp, txs, box.copy(proposition = generator.publicImage), attach, generator)

  lazy val powHeaderGen: Gen[PowBlockHeader] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    prevPosId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    brothersCount: Byte <- positiveByteGen
    brothersHash: Array[Byte] <- genBytesList(Blake2b256.DigestSize)
    prop: PublicKey25519Proposition <- propositionGen
  } yield new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, prop)

  lazy val powBlockGen: Gen[PowBlock] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    prevPosId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    brothersCount: Byte <- positiveByteGen
    proposition: PublicKey25519Proposition <- propositionGen
    brothers <- Gen.listOfN(brothersCount, powHeaderGen)
  } yield {
    val brotherBytes = PowBlockCompanion.brotherBytes(brothers)
    val brothersHash: Array[Byte] = Blake2b256(brotherBytes)
    new PowBlock(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, proposition, brothers)
  }


  lazy val noncedBoxGen: Gen[PublicKey25519NoncedBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield PublicKey25519NoncedBox(proposition, nonce, value)

  lazy val stateChangesGen: Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = noncedBoxGen
    .map(b => BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Seq(Insertion(b))))

  lazy val walletBoxGen: Gen[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = for {
    createdAt <- positiveLongGen
    txId <- genBytesList(NodeViewModifier.ModifierIdSize)
    box: PublicKey25519NoncedBox <- noncedBoxGen
  } yield WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, createdAt)(PublicKey25519NoncedBoxSerializer)

  def generateHistory: HybridHistory = {
    val dataDir = s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val storage = new HistoryStorage(blockStorage, settings)
    //we don't care about validation here
    val validators = Seq()

    var history = new HybridHistory(storage, settings, validators, None)

    val genesisBlock = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1478164225796L, -308545845552064644L,
      0, Array.fill(32)(0: Byte), PublicKey25519Proposition(scorex.utils.Random.randomBytes(32)), Seq())
    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }
}
