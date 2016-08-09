package scorex.block

import scorex.NodeStateHolder
import scorex.consensus.History
import scorex.transaction.{StateChanges, Transaction}
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState

/**
 * Calculates
 */
trait RewardCalculator[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData] {

  def reward(block: Block[P, TD, CD]): StateChanges[P]
}
