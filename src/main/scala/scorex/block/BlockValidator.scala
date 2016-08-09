package scorex.block

import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition

class BlockValidator[P <: Proposition, TData <: TransactionalData[_ <: Transaction[P, _]], CData <: ConsensusData]
(txValidator: TransactionalValidator[P, TData], cValidator: ConsensusValidator[CData]) {

  def isValid(block: Block[P, TData, CData]): Boolean = cValidator.isValid(block.consensusData) &&
    txValidator.isValid(block.transactionalData)

}

trait TransactionalValidator[P <: Proposition, TData <: TransactionalData[_ <: Transaction[P, _]]] {
  def isValid(tData: TData): Boolean

}

trait ConsensusValidator[CData <: ConsensusData] {
  def isValid(cData: CData): Boolean

}
