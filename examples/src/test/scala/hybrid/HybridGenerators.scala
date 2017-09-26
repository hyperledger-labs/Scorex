package hybrid

import commons.ExamplesCommonGenerators
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.{Nonce, Value}
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks._
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.HBoxStoredState
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.{ModifierId, NodeViewModifier}
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state._
import scorex.core.transaction.wallet.WalletBox
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Signature
import scorex.testkit.utils.{FileUtils, NoShrink}

import scala.util.Random

trait HybridGenerators extends ExamplesCommonGenerators
  with Settings
  with StoreGenerators
  with HistoryGenerators
  with StateGenerators
  with ModifierGenerators
  with NodeViewHolderGenerators
  with FileUtils
  with NoShrink {

  type ChangesGen = Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]]


  lazy val hybridSyncInfoGen: Gen[HybridSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    pos <- modifierIdGen
    pow <- modifierIdGen
    pows <- Gen.nonEmptyListOf(pow).map(_.take(HybridSyncInfo.MaxLastPowBlocks))
  } yield HybridSyncInfo(answer, pows, pos)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize)
    .map(g => Signature25519(Signature @@ g))

  lazy val blockIdGen: Gen[BlockId] = modifierIdGen

  lazy val blockIdsGen: Gen[Seq[BlockId]] = Gen.listOf(blockIdGen)

  lazy val nonEmptyBlockIdsGen: Gen[Seq[BlockId]] = Gen.nonEmptyListOf(blockIdGen)

  lazy val posBlockGen: Gen[PosBlock] = for {
    timestamp: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransaction] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionGen))
    box: PublicKey25519NoncedBox <- noncedBoxGen
    attach: Array[Byte] <- genBoundedBytes(0, 4096)
    generator: PrivateKey25519 <- key25519Gen.map(_._1)
    posParentId: ModifierId <- modifierIdGen
  } yield PosBlock.create(posParentId, timestamp, txs, box.copy(proposition = generator.publicImage), attach, generator)

  lazy val powHeaderGen: Gen[PowBlockHeader] = for {
    parentId: BlockId <- modifierIdGen
    prevPosId: BlockId <- modifierIdGen
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    brothersCount: Byte <- positiveByteGen
    brothersHash: Array[Byte] <- genBytesList(Blake2b256.DigestSize)
    prop: PublicKey25519Proposition <- propositionGen
  } yield new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, prop)

  lazy val powBlockGen: Gen[PowBlock] = for {
    parentId: BlockId <- modifierIdGen
    prevPosId: BlockId <- modifierIdGen
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
    nonce <- nonceGen
    value <- valueGen
  } yield PublicKey25519NoncedBox(proposition, nonce, value)

  lazy val noncedBoxWithKeyGen: Gen[(PublicKey25519NoncedBox, PrivateKey25519)] = for {
    pair <- key25519Gen
    nonce <- nonceGen
    value <- valueGen
  } yield PublicKey25519NoncedBox(pair._2, nonce, value) -> pair._1


  lazy val noncedBoxListGen: Gen[List[PublicKey25519NoncedBox]] = for {
    count <- smallInt.map(_ + 5)
    boxList <- Gen.listOfN(count, noncedBoxGen)
  } yield boxList

  lazy val noncedBoxWithKeyListGen: Gen[List[(PublicKey25519NoncedBox, PrivateKey25519)]] = for {
    count <- smallInt.map(_ + 50)
    boxList <- Gen.listOfN(count, noncedBoxWithKeyGen)
  } yield boxList


  lazy val walletBoxGen: Gen[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = for {
    createdAt <- positiveLongGen
    txId <- genBytesList(NodeViewModifier.ModifierIdSize)
    box: PublicKey25519NoncedBox <- noncedBoxGen
  } yield WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, createdAt)(PublicKey25519NoncedBoxSerializer)

  //Generators
  val transactionGenerator: Gen[SimpleBoxTransaction] = simpleBoxTransactionGen

  def stateChangesGenerator(state: HBoxStoredState): ChangesGen = {
    val removals: List[Removal[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
      state.store.getAll().take(5).map(_._2).map(_.data)
        .map(PublicKey25519NoncedBoxSerializer.parseBytes).map(_.get).toList
        .map(b => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](b.id))

    noncedBoxListGen.map { boxesToAdd: List[PublicKey25519NoncedBox] =>
      val insertions = boxesToAdd.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b))
      val ops = scala.util.Random.shuffle(removals ++ insertions)
      BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
    }
  }

  def genValidTransactionPair(state: HBoxStoredState): Seq[SimpleBoxTransaction] = {
    val keys = key25519Gen.apply(Gen.Parameters.default, Seed.random()).get
    val value = valueGen.apply(Gen.Parameters.default, Seed.random()).get

    val newBox: IndexedSeq[(PublicKey25519Proposition, Value)] = IndexedSeq((keys._2, value))
    val trx: SimpleBoxTransaction = simpleBoxTransactionGenCustomMakeBoxes(newBox).apply(Gen.Parameters.default, Seed.random()).get
    val useBox: IndexedSeq[(PrivateKey25519, Nonce)] = IndexedSeq((keys._1, trx.newBoxes.toVector(0).nonce))

    var trxnPair = Seq[SimpleBoxTransaction]()
    trxnPair = trxnPair :+ trx
    trxnPair = trxnPair :+ simpleBoxTransactionGenCustomUseBoxes(useBox).apply(Gen.Parameters.default, Seed.random()).get

    trxnPair
  }

  def semanticallyValidModifierWithCustomTransactions(state: HBoxStoredState,
                                                      transactions: Seq[SimpleBoxTransaction]): PosBlock = {
    for {
      id <- modifierIdGen
      timestamp: Long <- positiveLongGen
      txs: Seq[SimpleBoxTransaction] = transactions
      box: PublicKey25519NoncedBox <- noncedBoxGen
      attach: Array[Byte] <- genBoundedBytes(0, 4096)
      generator: PrivateKey25519 <- key25519Gen.map(_._1)
    } yield PosBlock.create(id, timestamp, txs, box.copy(proposition = generator.publicImage), attach, generator)
  }.apply(Gen.Parameters.default, Seed.random()).get


  def modifierWithTransactions(memoryPoolOpt: Option[SimpleBoxTransactionMemPool],
                               customTransactionsOpt: Option[Seq[SimpleBoxTransaction]]): PosBlock = {

    val (id, timestamp, box, attach, generator) = (for {
      id <- modifierIdGen
      timestamp: Long <- positiveLongGen
      generator: PrivateKey25519 <- key25519Gen.map(_._1)
      box: PublicKey25519NoncedBox <- noncedBoxGen.map(_.copy(proposition = generator.publicImage))
      attach: Array[Byte] <- genBoundedBytes(0, 4096)
    } yield (id, timestamp, box, attach, generator)).apply(Gen.Parameters.default, Seed.random()).get

    val txs = memoryPoolOpt.map { memPool =>
      val toTake = Random.nextInt(memPool.size)
      Random.shuffle(memPool.take(memPool.size).toSeq).take(toTake)
    }.getOrElse(Seq()) ++ customTransactionsOpt.getOrElse(Seq()) match {
      case s if s.isEmpty => simpleBoxTransactionsGen.sample.get
      case s => s
    }

    PosBlock.create(id, timestamp, txs, box, attach, generator)
  }

  def privKey(value: Long): (PrivateKey25519, PublicKey25519Proposition) =
    PrivateKey25519Companion.generateKeys(("secret_" + value).getBytes)
}
