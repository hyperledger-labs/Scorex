package scorex.core.block

import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState

/**
  * Calculate fees.
  */
trait StateChangesCalculator[P <: Proposition, TX <: Transaction[P, TX]] {
  def changes(block: Block[P, TX], state: MinimalState[P, TX]): StateChanges[P]
}

case class StateChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]])
