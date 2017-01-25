package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{MinimalState, StateChanges}

/**
  * The idea of this class is to get some generators and test some situations, common for all blockchains
  */
trait BlockchainSanity[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  type HT = History[P, TX, PM, SI, _ <: History[P, TX, PM, SI, _]]
  type ST = MinimalState[P, B, TX, PM, _ <: MinimalState[P, B, TX, PM, _]]

  private val hs = new HistorySanity[P, TX, PM, SI]
  private val ss = new StateSanity[P, TX, PM, SI, B]

  val history: HT
  val state: ST
  val blockGenerator: Gen[PM]
  val stateChangesGenerator: Gen[StateChanges[P, B]]

  property("appended block is in history") {
    hs.appendedBlockIsInHistory(history, blockGenerator)
  }
  property("State should be able to add a box") {
    ss.appendedBoxesAreInState(state, stateChangesGenerator)
  }


}
