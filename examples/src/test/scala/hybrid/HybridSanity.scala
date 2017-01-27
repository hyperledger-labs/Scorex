package hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.StateChanges
import scorex.testkit.BlockchainSanity

class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  HMemPool] with HybridGenerators {

  //Node view components
  override val history = generateHistory
  override val mempool: HMemPool = HMemPool.emptyPool
  override val wallet = (0 until 100).foldLeft(HWallet.readOrGenerate(settings, "p"))((w, _) => w.generateNewSecret())
  override val state = HBoxStoredState.readOrGenerate(settings)

  //Generators
  override val transactionGenerator: Gen[SimpleBoxTransaction] = simpleBoxTransactionGen
  private val validPowBlockGen: Gen[PowBlock] = powBlockGen.map(b => b.copy(parentId = history.bestPowId, prevPosId = history.bestPosId))
  private val validPosBlockGen: Gen[PosBlock] = posBlockGen.map(b => b.copy(parentId = history.bestPowId))

  override val validBlockGenerator: Gen[HybridBlock] = for {
    posB <- validPosBlockGen
    powB <- validPowBlockGen
  } yield if (history.pairCompleted) powB else posB


  override val stateChangesGenerator: Gen[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    stateChangesGen



}
