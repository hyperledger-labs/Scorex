package hybrid

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, StateChanges}
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}

class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory] with BlockchainPerformance[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory] with HybridGenerators {

  //Node view components
  override val history = generateHistory
  override val mempool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool.emptyPool
  override val wallet = (0 until 100).foldLeft(HWallet.readOrGenerate(settings, "p"))((w, _) => w.generateNewSecret())
  override val state = HBoxStoredState.readOrGenerate(settings)

  //Generators
  override val transactionGenerator: Gen[SimpleBoxTransaction] = simpleBoxTransactionGen

  override val stateChangesGenerator: Gen[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    stateChangesGen

  override def genValidModifier(curHistory: HybridHistory): HybridBlock = {
    if (curHistory.pairCompleted) {
      for {
        timestamp: Long <- positiveLongGen
        nonce: Long <- positiveLongGen
        brothersCount: Byte <- positiveByteGen
        proposition: PublicKey25519Proposition <- propositionGen
        brothers <- Gen.listOfN(brothersCount, powHeaderGen)
      } yield {
        val brotherBytes = PowBlockCompanion.brotherBytes(brothers)
        val brothersHash: Array[Byte] = FastCryptographicHash(brotherBytes)
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

}
