package scorex.block

import scorex.transaction.box.proposition.Proposition
import scorex.transaction.{StateChanges, Transaction}

/**
 * Calculates
 */
trait RewardCalculator[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData] {

  def reward(block: Block[P, TD, CD]): StateChanges[P]
}
