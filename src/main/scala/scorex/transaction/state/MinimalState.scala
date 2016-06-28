package scorex.transaction.state

import scorex.block.Block
import scorex.transaction.Transaction
import scorex.transaction.box.{Box, Proposition}
import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[P <: Proposition, TX <: Transaction[P, TX]] {
  def version: Int

  def isValid(tx: TX): Boolean = tx.validate(this).isSuccess

  def areValid(txs: Seq[TX]): Boolean = txs.forall(isValid)

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def processBlock(block: Block[P, _, _]): Try[Unit]

  def rollbackTo(height: Int): Try[Unit]

  def closedBox(boxId: Array[Byte]): Option[Box[P]]
}