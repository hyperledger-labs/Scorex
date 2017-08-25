package hybrid

import java.io.File

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.iohk.iodb.LSMStore
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion, PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.hash.Blake2b256
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}

import scala.util.Random


class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  PosBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory] with BlockchainPerformance[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory]
  with HybridGenerators {

  private val dir = s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}"
  private val f = new File(dir)
  f.mkdirs()

  private val store = new LSMStore(f)

  //Node view components
  override val history: HybridHistory = generateHistory
  override val memPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool.emptyPool
  override val wallet = (0 until 100).foldLeft(HWallet.readOrGenerate(settings, "p"))((w, _) => w.generateNewSecret())


  private val hf = Blake2b256

  private def privKey(value: Long) = PrivateKey25519Companion.generateKeys(("secret" + value).getBytes)

  private def randomBox(): PublicKey25519NoncedBox = {
    val value = Random.nextInt(400000) + 200000
    PublicKey25519NoncedBox(privKey(value)._2, Random.nextLong(), value)
  }

  override val state: HBoxStoredState = {
    val s0 = HBoxStoredState(store, modifierIdGen.sample.get)
    val inserts = (1 to 200000).map(_ => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](randomBox()))
    s0.applyChanges(BoxStateChanges(inserts), modifierIdGen.sample.get).get
  }

  //Generators
  override val transactionGenerator: Gen[SimpleBoxTransaction] = simpleBoxTransactionGen

  override val stateChangesGenerator: Gen[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    stateChangesGen


  override def semanticallyValidModifier(state: HBoxStoredState): HybridBlock =
    semanticallyValidModifierWithTransactions(state)

  override def totallyValidModifier(history: HybridHistory, state: HBoxStoredState): HybridBlock = ???

  override def syntacticallyValidModifier(curHistory: HybridHistory): HybridBlock = {

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
  }.apply(Gen.Parameters.default, Seed.random()).get

  override def syntacticallyInvalidModifier(curHistory: HybridHistory): HybridBlock = {
    syntacticallyValidModifier(curHistory) match {
      case pow: PowBlock => pow.copy(parentId = hf(pow.parentId))
      case pos: PosBlock => pos.copy(parentId = hf(pos.parentId))
    }
  }

  override def semanticallyValidModifierWithTransactions(state: HBoxStoredState): PosBlock = {
    val (txCount, insPerTx, timestamp, attach) = (for {
      txCount <- smallInt
      insPerTx <- smallInt.map(_ + 1)
      timestamp: Long <- positiveLongGen
      attach: Array[Byte] <- genBoundedBytes(0, 4096)
    } yield (txCount, insPerTx, timestamp, attach)).sample.get

    val txs = state.store.getAll().take(txCount * insPerTx + 1).map(_._2).toSeq.tail
      .map(_.data).map(PublicKey25519NoncedBoxSerializer.parseBytes).map(_.get).grouped(insPerTx).map{inputs =>

      val from = inputs.map(i => privKey(i.value)._1 -> i.nonce).toIndexedSeq
      val to = inputs.map(i => i.proposition -> (i.value - 1)).toIndexedSeq

      SimpleBoxTransaction(from, to, fee = inputs.size, System.currentTimeMillis())
    }.toSeq

    val genBox:PublicKey25519NoncedBox =
      PublicKey25519NoncedBoxSerializer.parseBytes(state.store.getAll().next()._2.data).get

    val generator = privKey(genBox.value)._1

    PosBlock.create(state.version, timestamp, txs, genBox, attach, generator)
  }

  override def genValidTransactionPair(state: HBoxStoredState): Seq[SimpleBoxTransaction] = {
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

  override def semanticallyValidModifierWithCustomTransactions(state: HBoxStoredState,
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


  override def modifierWithTransactions(memoryPoolOpt: Option[SimpleBoxTransactionMemPool],
                                        customTransactionsOpt: Option[Seq[SimpleBoxTransaction]]): PosBlock = {

    val (id, timestamp, box, attach, generator) = (for {
      id <- modifierIdGen
      timestamp: Long <- positiveLongGen
      generator: PrivateKey25519 <- key25519Gen.map(_._1)
      box: PublicKey25519NoncedBox <- noncedBoxGen.map(_.copy(proposition = generator.publicImage))
      attach: Array[Byte] <- genBoundedBytes(0, 4096)
    } yield (id, timestamp, box, attach, generator)).apply(Gen.Parameters.default, Seed.random()).get

    val txs = memoryPoolOpt.map{ memPool =>
      val toTake = Random.nextInt(memPool.size)
      Random.shuffle(memPool.take(memPool.size).toSeq).take(toTake)
    }.getOrElse(Seq()) ++ customTransactionsOpt.getOrElse(Seq()) match {
      case s if s.isEmpty => simpleBoxTransactionsGen.sample.get
      case s => s
    }

    PosBlock.create(id, timestamp, txs, box, attach, generator)
  }
}