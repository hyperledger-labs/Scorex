package scorex.core.consensus

import scorex.core.block.Block
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait BlockChain[P <: Proposition, TX <: Transaction[P, TX], B <: Block[P, TX]]
  extends History[P, TX, B] with ScorexLogging {

  type Score = BigInt

  def score(block: B): Score

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
  def heightOf(block: B): Option[Int] = heightOf(block.id)

  def heightOf(blockId: BlockId): Option[Int]

  def confirmations(block: B): Option[Int] = heightOf(block).map(height() - _)

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: Block[P, TX] = lastBlocks(1).head

  def lastBlockIds(howMany: Int): Seq[BlockId] = lastBlocks(howMany).map(_.id)

  //just last block id
  override def openSurface(): scala.Seq[BlockId] = lastBlockIds(1)

  override def continuationIds(openSurface: Seq[BlockId], size: Int): Seq[BlockId] = lastBlockIds(size)

  /**
    * Average delay in milliseconds between last blockNum blocks starting from block
    */
  def averageDelay(blockId: BlockId, blockNum: Int): Try[Long] = Try {
    val block = blockById(blockId).get
    (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
  }

  def discardBlock(): Try[BlockChain[P, TX, B]]

  def blockAt(height: Int): Option[B]

  def parent(block: B, back: Int = 1): Option[B] = {
    require(back > 0)
    heightOf(block.parentId).flatMap(referenceHeight => blockAt(referenceHeight - back + 1))
  }

  def lastBlocks(howMany: Int): Seq[B] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(blockAt).reverse

  override def continuation(openSurface: Seq[BlockId], size: Int): Seq[B] = lastBlocks(size)

  /**
    * Return howMany blocks starting from parentSignature
    */
  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(_.id)
    }.getOrElse(Seq())

  def children(blockId: BlockId): Seq[B]

  lazy val genesisBlock: B = blockAt(1).get
}