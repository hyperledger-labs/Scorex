package scorex.block

import scorex.transaction.Transaction
import scorex.transaction.box.Box
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState

/**
  * Calculate fees.
  */
trait StateChangesCalculator[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData] {
  def changes(block: Block[P, TD, CD], state: MinimalState[P, TX]): StateChanges[P]
}

case class StateChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]])
