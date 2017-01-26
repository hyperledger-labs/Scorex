package hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import org.scalacheck.Gen
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, StateChanges}
import scorex.testkit.BlockchainSanity

class HibridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox] with HybridGenerators {

  override val history = generateHistory

  private val validPowBlockGen: Gen[PowBlock] = powBlockGen.map(b => b.copy(parentId = history.bestPowId, prevPosId = history.bestPosId))
  private val validPosBlockGen: Gen[PosBlock] = posBlockGen.map(b => b.copy(parentId = history.bestPowId))

  override val validBlockGenerator: Gen[HybridBlock] = for {
    posB <- validPosBlockGen
    powB <- validPowBlockGen
  } yield if (history.pairCompleted) powB else posB


  override val stateChangesGenerator: Gen[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    stateChangesGen


  override val state = HBoxStoredState.readOrGenerate(settings)

}
