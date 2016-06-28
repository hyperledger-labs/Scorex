package scorex.consensus

import scorex.block.{ConsensusData, Block}
import scorex.transaction.box.Proposition
import scorex.utils.ScorexLogging

import scala.util.Try

trait BlockChain[P <: Proposition, CData <: ConsensusData, B <: Block[P, CData, _]] extends History[P, CData, B] with ScorexLogging {
  this : ConsensusModule[P, CData, B] =>

  def blockAt(height: Int): Option[B]

  override def parent(block: B, back: Int = 1): Option[B] = {
    require(back > 0)
    heightOf(parentId(block)).flatMap(referenceHeight => blockAt(referenceHeight - back + 1))
  }

  override def discardBlock(): Try[Unit]

  override def lastBlocks(howMany: Int): Seq[B] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(blockAt).reverse

  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(id)
    }.getOrElse(Seq())

  def children(blockId: BlockId): Seq[B]

  override lazy val genesisBlock: B = blockAt(1).get
}