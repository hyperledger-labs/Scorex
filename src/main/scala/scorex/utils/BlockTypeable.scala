package scorex.utils

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition
import shapeless.Typeable

class BlockTypeable[P <: Proposition, TData <: TransactionalData[_ <: Transaction[P, _]], CData <: ConsensusData]
  extends Typeable[Block[P, TData, CData]] {
  def cast(t: Any): Option[Block[P, TData, CData]] = t match {
    case b: Block[P, TData, CData] => Some(b)
    case _ => None
  }

  def describe: String = "Block[P, TData, CData]"

  override def toString: String = s"Typeable[$describe]"
}