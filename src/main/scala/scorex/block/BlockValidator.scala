package scorex.block

import scorex.NodeStateHolder
import scorex.consensus.History
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState

class BlockValidator[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
(txValidator: TransactionalValidator[P, TX, TData], cValidator: ConsensusValidator[CData]) {

  def isValid(block: Block[P, TData, CData], state: NodeStateHolder[P, TX, TData, CData]): Boolean = {
    if (state.history.isEmpty) true
    else cValidator.isValid(block.consensusData, state.stableState._2) &&
      txValidator.isValid(block.transactionalData, state.stableState._1)
  }
}

trait TransactionalValidator[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX]] {
  def isValid(tData: TData, state: MinimalState[P, TX]): Boolean

}

trait ConsensusValidator[CData <: ConsensusData] {
  def isValid(cData: CData, history: History[_, _, _, CData]): Boolean

}
