package hybrid

import java.io.File

import commons.ExamplesCommonGenerators
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks._
import examples.hybrid.history.{HistoryStorage, HybridHistory, HybridSyncInfo}
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.HBoxStoredState
import io.circe
import io.iohk.iodb.LSMStore
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.NodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state._
import scorex.core.transaction.wallet.WalletBox
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._
import scala.util.Random

trait HybridGenerators extends ExamplesCommonGenerators {
  type ChangesGen = Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]]

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

  lazy val noncedBoxWithKeyGen: Gen[(PublicKey25519NoncedBox, PrivateKey25519)] = for {
    pair <- key25519Gen
    nonce <- positiveLongGen
    value <- positiveLongGen
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


  private val hf = Blake2b256

  lazy val singlePrivKey = PrivateKey25519Companion.generateKeys("secret".getBytes)

  //todo: make by value again, check why fails
  private def privKey(value: Long) = singlePrivKey // PrivateKey25519Companion.generateKeys(("secret" + value).getBytes)

   val stateGen: Gen[HBoxStoredState] = {
    def randomBox(): PublicKey25519NoncedBox = {
      val value = Random.nextInt(5000000) + 5000000
      val nonce = Random.nextLong()
      val keyPair = privKey(value)
      PublicKey25519NoncedBox(keyPair._2, nonce, value)
        .ensuring(box => PrivateKey25519Companion.owns(keyPair._1, box))
    }

    val dir = s"/tmp/scorex/scorextest/${System.currentTimeMillis()}/${Random.nextInt(1000)}"
    val f = new File(dir)
    f.mkdirs()
    val store = new LSMStore(f)

    val s0 = HBoxStoredState(store, modifierIdGen.sample.get)
    val inserts = (1 to 5000).map(_ => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](randomBox()))
    s0.applyChanges(BoxStateChanges(inserts), modifierIdGen.sample.get).get
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
      case pow: PowBlock => pow.copy(parentId = hf(pow.parentId))
      case pos: PosBlock => pos.copy(parentId = hf(pos.parentId))
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

    val stateBoxes = state.store.getAll().take(txCount * insPerTx + 1).map(_._2).toSeq
      .map(_.data).map(PublicKey25519NoncedBoxSerializer.parseBytes).map(_.get)

    assert(stateBoxes.size == txCount * insPerTx + 1)
    stateBoxes.tail.foreach(b => if(!PrivateKey25519Companion.owns(privKey(b.value)._1, b)){
      println("b: " + b)
      println(stateBoxes.head.proposition)
//        assert(false)
    })

    //  println("stateBoxes ids: " + stateBoxes.map(_.id).map(Base16.encode).mkString(" "))

    // println("stateBoxes: " + stateBoxes.mkString(" "))


    val txs = stateBoxes.tail.grouped(insPerTx).map { inputs =>
      val from = inputs.map(i => privKey(i.value)._1 -> i.nonce).toIndexedSeq
      val to = inputs.map(i => privKey(i.value)._2 -> (i.value)).toIndexedSeq
      SimpleBoxTransaction(from, to, fee = 0, System.currentTimeMillis())
    }.toSeq

    txs.foreach { tx =>
      tx.boxIdsToOpen.foreach{id =>
        //        println("tx box: " + Base16.encode(id))
        assert(state.closedBox(id).isDefined)
      }
    }

    val genBox: PublicKey25519NoncedBox = stateBoxes.head

    val generator = privKey(genBox.value)._1

    PosBlock.create(state.version, System.currentTimeMillis(), txs, genBox, attach, generator)
  }

   def genValidTransactionPair(state: HBoxStoredState): Seq[SimpleBoxTransaction] = {
    val keys = key25519Gen.apply(Gen.Parameters.default, Seed.random()).get
    val value = positiveLongGen.apply(Gen.Parameters.default, Seed.random()).get

    val newBox: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq((keys._2, value))
    val trx: SimpleBoxTransaction = simpleBoxTransactionGenCustomMakeBoxes(newBox).apply(Gen.Parameters.default, Seed.random()).get
    val useBox: IndexedSeq[(PrivateKey25519, Long)] = IndexedSeq((keys._1, trx.newBoxes.toVector(0).nonce))

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
