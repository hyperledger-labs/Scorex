package hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.StateChanges
import scorex.testkit.BlockchainSanity

class HibridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox] with HybridGenerators {

  override val history = generateHistory
  override val blockGenerator: Gen[HybridBlock] = for {
    posB <- posBlockGen
    powB <- powBlockGen
  } yield {
    if (history.pairCompleted) powB.copy(parentId = history.bestPowId, prevPosId = history.bestPosId)
    else posB.copy(parentId = history.bestPowId)
  }

  override val stateChangesGenerator: Gen[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    stateChangesGen


  override val state = HBoxStoredState.readOrGenerate(settings)

}
