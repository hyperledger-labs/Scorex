package hybrid

import examples.commons.SimpleBoxTransaction
import examples.curvepos.Nonce
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.hash.Blake2b256
import scorex.testkit.generators.CoreGenerators

trait ModifierGenerators { this: HybridGenerators with CoreGenerators =>

  private val hf = Blake2b256

  val txCountGen: Gen[Int] = Gen.chooseNum(0, 30)
  val insPerTxCountGen: Gen[Int] = Gen.chooseNum(1, 20)
  val attachGen: Gen[Array[Byte]] = genBoundedBytes(0, 4096)

  val txGen: Gen[(Int, Int, Array[Byte])] = for {
    tx <- txCountGen
    in <- insPerTxCountGen
    at <- attachGen
  } yield (tx, in, at)

  def semanticallyValidModifier(state: HBoxStoredState): PosBlock = {
    val (txCount, insPerTx, attach) = txGen.sample.get

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
    val version = ModifierId @@ state.version

    PosBlock.create(version, System.currentTimeMillis(), txs, genBox, attach, generator)
  }

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
        attach: Array[Byte] <- attachGen
        generator: PrivateKey25519 <- key25519Gen.map(_._1)
      } yield PosBlock.create(curHistory.bestPowId, timestamp, txs, box.copy(proposition = generator.publicImage), attach, generator)
    }
  }.sample.get

  def totallyValidModifier(history: HybridHistory, state: HBoxStoredState): HybridBlock = {
    val synBlock = syntacticallyValidModifier(history)
    val semBlock = semanticallyValidModifier(state)
    synBlock.asInstanceOf[PosBlock].copy(transactions = semBlock.transactions)
  }

  def semanticallyInvalidModifier(state: HBoxStoredState): HybridBlock = {
    val posBlock: PosBlock = semanticallyValidModifier(state)
    posBlock.transactions.lastOption.map { lastTx =>
      val modifiedFrom = (lastTx.from.head._1, Nonce @@ (lastTx.from.head._2 + 1)) +: lastTx.from.tail
      val modifiedLast = lastTx.copy(from = modifiedFrom)
      posBlock.copy(transactions = posBlock.transactions.dropRight(1) :+ modifiedLast)
    }.getOrElse {
      val modifiedGenerator = posBlock.generatorBox.copy(nonce = Nonce @@ (posBlock.generatorBox.nonce + 1))
      posBlock.copy(generatorBox = modifiedGenerator)
    }
  }

  def syntacticallyInvalidModifier(curHistory: HybridHistory): HybridBlock = {
    syntacticallyValidModifier(curHistory) match {
      case pow: PowBlock => pow.copy(parentId = ModifierId @@ hf(pow.parentId))
      case pos: PosBlock => pos.copy(parentId = ModifierId @@ hf(pos.parentId))
    }
  }
}
