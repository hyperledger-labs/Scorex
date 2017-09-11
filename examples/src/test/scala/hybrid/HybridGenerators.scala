package hybrid

import commons.ExamplesCommonGenerators
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.{Nonce, Value}
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks._
import examples.hybrid.history.{HistoryStorage, HybridHistory, HybridSyncInfo}
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.HBoxStoredState
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.{ModifierId, NodeViewModifier}
import scorex.core.block.Block._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state._
import scorex.core.transaction.wallet.WalletBox
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.testkit.utils.{FileUtils, NoShrink}

import scala.concurrent.duration._
import scala.util.Random

trait HybridGenerators extends ExamplesCommonGenerators with FileUtils with NoShrink {
  type ChangesGen = Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]]

  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")

    override lazy val targetBlockDelay: Long = 3.seconds.toMillis

    override lazy val Difficulty: BigInt = 1
  }

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

  def generateHistory: HybridHistory = {
    val blockStorage = new LSMStore(createTempDir)
    val storage = new HistoryStorage(blockStorage, settings)
    //we don't care about validation here
    val validators = Seq()

    var history = new HybridHistory(storage, settings, validators, None)

    val genesisBlock = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1478164225796L, -308545845552064644L,
      0, Array.fill(32)(0: Byte), PublicKey25519Proposition(PublicKey @@ scorex.utils.Random.randomBytes(32)), Seq())
    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }


  private val hf = Blake2b256

  private val valueSeed = 5000000

  //todo: make by value again, check why fails
  private def privKey(value: Long) = PrivateKey25519Companion.generateKeys(("secret_" + value).getBytes)

  val stateGen: Gen[HBoxStoredState] = tempDirGen.map { dir =>
    def randomBox(): PublicKey25519NoncedBox = {
      val value: Value = Value @@ (Random.nextInt(valueSeed) + valueSeed).toLong
      val nonce: Nonce = Nonce @@ Random.nextLong()
      val keyPair = privKey(value)
      PublicKey25519NoncedBox(keyPair._2, nonce, value)
        .ensuring(box => PrivateKey25519Companion.owns(keyPair._1, box))
    }

    val store = new LSMStore(dir)
    val s0 = HBoxStoredState(store, versionTagGen.sample.get)
    val inserts = (1 to 5000).map(_ => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](randomBox()))
    s0.applyChanges(BoxStateChanges(inserts), versionTagGen.sample.get).get
  }

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

  def semanticallyValidModifier(state: HBoxStoredState): HybridBlock =
    semanticallyValidModifierWithTransactions(state)

  def semanticallyInvalidModifier(state: HBoxStoredState): HybridBlock = {
    val posBlock: PosBlock = semanticallyValidModifierWithTransactions(state)
    posBlock.transactions.lastOption.map { lastTx =>
      val modifiedFrom = (lastTx.from.head._1, Nonce @@ (lastTx.from.head._2 + 1)) +: lastTx.from.tail
      val modifiedLast = lastTx.copy(from = modifiedFrom)
      posBlock.copy(transactions = posBlock.transactions.dropRight(1) :+ modifiedLast)
    }.getOrElse {
      val modifiedGenerator = posBlock.generatorBox.copy(nonce = Nonce @@ (posBlock.generatorBox.nonce + 1))
      posBlock.copy(generatorBox = modifiedGenerator)
    }
  }


  def totallyValidModifier(history: HybridHistory, state: HBoxStoredState): HybridBlock = ???

  def syntacticallyValidModifier(curHistory: HybridHistory): HybridBlock = {

    if (curHistory.pairCompleted) {
      for {
        timestamp: Long <- positiveLongGen
        nonce: Long <- positiveLongGen
        brothersCount: Byte <- positiveByteGen
        proposition: PublicKey25519Proposition <- propositionGen
        brothers <- Gen.listOfN(brothersCount, powHeaderGen)
      } yield {
        val brotherBytes = PowBlockCompanion.brotherBytes(brothers)
        val brothersHash: Array[Byte] = Blake2b256(brotherBytes)
        new PowBlock(curHistory.bestPowId, curHistory.bestPosId, timestamp, nonce, brothersCount, brothersHash, proposition, brothers)
      }
    } else {
      for {
        timestamp: Long <- positiveLongGen
        txs: Seq[SimpleBoxTransaction] <- smallInt.flatMap(txNum => Gen.listOfN(txNum, simpleBoxTransactionGen))
        box: PublicKey25519NoncedBox <- noncedBoxGen
        attach: Array[Byte] <- genBoundedBytes(0, 4096)
        generator: PrivateKey25519 <- key25519Gen.map(_._1)
      } yield PosBlock.create(curHistory.bestPowId, timestamp, txs, box.copy(proposition = generator.publicImage), attach, generator)
    }
  }.sample.get

  def syntacticallyInvalidModifier(curHistory: HybridHistory): HybridBlock = {
    syntacticallyValidModifier(curHistory) match {
      case pow: PowBlock => pow.copy(parentId = ModifierId @@ hf(pow.parentId))
      case pos: PosBlock => pos.copy(parentId = ModifierId @@ hf(pos.parentId))
    }
  }

  def semanticallyValidModifierWithTransactions(state: HBoxStoredState): PosBlock = {
    val (txCount, insPerTx, attach) = (for {
      txCount <- Gen.choose(0, 30)
      insPerTx <- Gen.choose(1, 20)
      attach: Array[Byte] <- genBoundedBytes(0, 4096)
    } yield (txCount, insPerTx, attach)).sample.get

    assert(txCount >= 0 && txCount <= 30)
    assert(insPerTx >= 1 && insPerTx <= 20)

    def filterOutForgedBoxe(in: (ByteArrayWrapper, ByteArrayWrapper)): Boolean = {
      PublicKey25519NoncedBoxSerializer.parseBytes(in._2.data).map(_.value).getOrElse(0L) > 0
    }

    val stateBoxes = state.store.getAll()
      .filter(filterOutForgedBoxe)
      .take(txCount * insPerTx + 1)
      .map { case (_, wrappedData) => PublicKey25519NoncedBoxSerializer.parseBytes(wrappedData.data).get }
      .toSeq

    assert(stateBoxes.size == txCount * insPerTx + 1)

    val txs = stateBoxes.tail.grouped(insPerTx).map { inputs =>
      val fee = 0
      val from = inputs.map(i => privKey(i.value)._1 -> i.nonce).toIndexedSeq
      val to = inputs.map(i => privKey(i.value)._2 -> i.value).toIndexedSeq
      SimpleBoxTransaction(from, to, fee = fee, System.currentTimeMillis())
    }.toSeq

    txs.foreach {
      _.boxIdsToOpen.foreach { id => assert(state.closedBox(id).isDefined) }
    }

    val genBox: PublicKey25519NoncedBox = stateBoxes.head
    val generator = privKey(genBox.value)._1
    PosBlock.create(ModifierId @@ state.version, System.currentTimeMillis(), txs, genBox, attach, generator)
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
}
