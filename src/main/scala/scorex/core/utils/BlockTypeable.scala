package scorex.core.utils

import scorex.core.block.Block
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import shapeless.Typeable

class BlockTypeable[P <: Proposition, TX <: Transaction[P]]
  extends Typeable[Block[P, TX]] {
  def cast(t: Any): Option[Block[P, TX]] = t match {
    case b: Block[P, TX] => Some(b)
    case _ => None
  }

  def describe: String = "Block[P <: Proposition, TX <: Transaction[P]]"

  override def toString: String = s"Typeable[$describe]"
}