package scorex.consensus

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition
import scorex.utils.ScorexLogging

import scala.util.Try

trait BlockChain[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData] extends History[P, TX, TData, CData] with ScorexLogging {
  this: ConsensusModule[P, TX, TData, CData] =>

  def blockAt(height: Int): Option[Block[P, CData, TData]]

  override def parent(block: Block[P, CData, TData], back: Int = 1): Option[Block[P, CData, TData]] = {
    require(back > 0)
    heightOf(parentId(block)).flatMap(referenceHeight => blockAt(referenceHeight - back + 1))
  }

  override def discardBlock(): Try[Unit]

  override def lastBlocks(howMany: Int): Seq[Block[P, CData, TData]] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(blockAt).reverse

  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(id)
    }.getOrElse(Seq())

  def children(blockId: BlockId): Seq[Block[P, CData, TData]]

  override lazy val genesisBlock: Block[P, CData, TData] = blockAt(1).get
}
