package scorex.core.consensus

import scorex.core.ModifierTypeId
import scorex.core.block.Block
import scorex.core.transaction.Transaction
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.Try

trait BlockChain[TX <: Transaction, B <: Block[TX], SI <: SyncInfo, BT <: BlockChain[TX, B, SI, BT]]
  extends History[B, SI, BT] with ScorexLogging {

  import BlockChain.Score

  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  def height(): Int

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

  def heightOf(blockId: ModifierId): Option[Int]

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: Option[B] = lastBlocks(1).headOption

  def lastBlockIds(howMany: Int): Seq[ModifierId] = lastBlocks(howMany).map(_.id)

  //just last block id
  override def openSurfaceIds(): scala.Seq[ModifierId] = lastBlockIds(1)

  override def continuationIds(info: SI, size: Int): Seq[(ModifierTypeId, ModifierId)] = {
    val openSurface = info.startingPoints
    require(openSurface.size == 1)
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val modId = openSurface.head._1
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val s = lookForward(openSurface.head._2, size)
    s.map(id => modId -> id)
  }

  /**
    * Average delay in milliseconds between last blockNum blocks starting from block
    */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def averageDelay(blockId: ModifierId, blockNum: Int): Try[Long] = Try {
    val block = modifierById(blockId).get
    (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
  }

  def blockAt(height: Int): Option[B]

  def parent(block: B, back: Int = 1): Option[B] = {
    require(back > 0)
    heightOf(block.parentId).flatMap(referenceHeight => blockAt(referenceHeight - back + 1))
  }

  def lastBlocks(howMany: Int): Seq[B] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(blockAt).reverse

  /**
    * Return howMany blocks starting from parentSignature
    */
  def lookForward(parentSignature: ModifierId, howMany: Int): Seq[ModifierId] = heightOf(parentSignature).map { h =>
    (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(_.id)
  }.getOrElse(Seq())

  def children(blockId: ModifierId): Seq[B]
  
  /**
    * Score of concrete block
    */
  def score(block: B): Score

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    */
  def chainScore(): Score
}

object BlockChain {
  type Score = BigInt
}
