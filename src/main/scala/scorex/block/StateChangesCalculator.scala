package scorex.block

import scorex.NodeStateHolder
import scorex.transaction.Transaction
import scorex.transaction.box.Box
import scorex.transaction.box.proposition.Proposition

/**
  * Calculate fees.
  */
trait StateChangesCalculator[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData] {
  def changes(block: Block[P, TD, CD], state: NodeStateHolder[P, TX, TD, CD]): StateChanges[P]
}

case class StateChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]])
