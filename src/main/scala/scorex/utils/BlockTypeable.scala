package scorex.utils

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition
import shapeless.Typeable

class BlockTypeable[P <: Proposition, CData <: ConsensusData, TData <: TransactionalData[_ <: Transaction[P, _]]] extends Typeable[Block[P, CData, TData]] {
  def cast(t: Any): Option[Block[P, CData, TData]] = t match {
    case b: Block[P, CData, TData] => Some(b)
    case _ => None
  }

  def describe: String = "Block[P, CData, TData]"

  override def toString: String = s"Typeable[$describe]"
}