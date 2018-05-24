package scorex.core.utils

import scorex.core.block.Block
import scorex.core.transaction.Transaction
import shapeless.Typeable

class BlockTypeable[TX <: Transaction] extends Typeable[Block[TX]] {

  def cast(t: Any): Option[Block[TX]] = t match {
    case b: Block[TX] => Some(b)
    case _ => None
  }

  def describe: String = "Block[TX <: Transaction]"

  override def toString: String = s"Typeable[$describe]"
}