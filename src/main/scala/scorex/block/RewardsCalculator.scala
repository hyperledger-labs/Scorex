package scorex.block

import scorex.transaction.box.proposition.Proposition
import scorex.transaction.{StateChanges, Transaction}

/**
 * Calculate fees.
 */
trait RewardsCalculator[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData] {
  def rewards(block: Block[P, TD, CD]): StateChanges[P]
}
