package scorex.core.consensus

import scorex.core.block.Block
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait BlockChain[P <: Proposition, TX <: Transaction[P, TX]]
  extends History[P, TX] with ScorexLogging {

  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  def height(): Int

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    *
    * @return
    */
  def score(): BigInt

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height() == 0

  /**
    * Height of a block if it's in the blocktree
    */
  def heightOf(block: Block[P, TX]): Option[Int] = heightOf(block.id)

  def heightOf(blockId: BlockId): Option[Int]

  def confirmations(block: Block[P, TX]): Option[Int] = heightOf(block).map(height() - _)

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: Block[P, TX] = lastBlocks(1).head

  def lastBlockIds(howMany: Int): Seq[BlockId] = lastBlocks(howMany).map(_.id)

  /**
    * Average delay in milliseconds between last blockNum blocks starting from block
    */
  def averageDelay(blockId: BlockId, blockNum: Int): Try[Long] = Try {
    val block = blockById(blockId).get
    (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
  }

  def discardBlock(): Try[History[P, TX]]

  def blockAt(height: Int): Option[Block[P, TX]]

  def parent(block: Block[P, TX], back: Int = 1): Option[Block[P, TX]] = {
    require(back > 0)
    heightOf(block.parentId).flatMap(referenceHeight => blockAt(referenceHeight - back + 1))
  }

  def lastBlocks(howMany: Int): Seq[Block[P, TX]] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(blockAt).reverse

  /**
    * Return howMany blocks starting from parentSignature
    */
  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(_.id)
    }.getOrElse(Seq())

  def children(blockId: BlockId): Seq[Block[P, TX]]

  lazy val genesisBlock: Block[P, TX] = blockAt(1).get
}