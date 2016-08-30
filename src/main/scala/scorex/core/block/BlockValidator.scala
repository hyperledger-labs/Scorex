package scorex.core.block

import scorex.core.consensus.History
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState

/*class BlockValidator[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
(txValidator: TransactionalValidator[P, TX, TData], cValidator: ConsensusValidator[CData]) {

  def isValid(block: Block[P, TData, CData], state: DefaultNodeStateHandler[P, TX, TData, CData]): Boolean = {
    if (state.history.isEmpty) true
    else cValidator.isValid(block.consensusData, state.stableState._2) &&
      txValidator.isValid(block.transactionalData, state.stableState._1)
  }
}*/