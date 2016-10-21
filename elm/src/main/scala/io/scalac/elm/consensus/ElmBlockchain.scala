package io.scalac.elm.consensus

import io.scalac.elm.consensus.ElmBlockchain.Height
import io.scalac.elm.transaction.{ElmBlock, ElmTransaction}
import scorex.core.NodeViewComponentCompanion
import scorex.core.consensus.BlockChain
import scorex.core.consensus.History.{BlockId, HistoryComparisonResult, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

object ElmBlockchain {
  type Height = Int
}

case class ElmBlockchain(blockIds: Map[Height, BlockId] = Map(), blocks: Map[BlockId, ElmBlock] = Map())
  extends BlockChain[PublicKey25519Proposition, ElmTransaction, ElmBlock, ElmSyncInfo, ElmBlockchain] {

  import BlockChain.Score

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[ElmBlock] =
    blocks.find(_._1.sameElements(blockId)).map(_._2)

  override def append(block: ElmBlock): Try[(ElmBlockchain, Option[RollbackTo[ElmBlock]])] = synchronized {
    val blockId = block.id
    val parentId = block.parentId

    if (blockIds.isEmpty || (lastBlock.id sameElements parentId)) {
      val h = height() + 1
      val newChain = ElmBlockchain(blockIds + (h -> blockId), blocks + (blockId -> block))
      Success(newChain, None)
    } else Failure(new Exception(s"Last block id is ${Base58.encode(blockIds.last._2)}, " +
      s"expected ${Base58.encode(parentId)}}"))
  }

  override def openSurface(): Seq[BlockId] = Seq(blocks.last._1)

  override def continuation(from: Seq[BlockId], size: Int): Seq[ElmBlock] =
    continuationIds(from, size).map(blockById).map(_.get)

  override def continuationIds(from: Seq[BlockId], size: Int): Seq[BlockId] = {
    require(from.size == 1)
    blocks.dropWhile(t => !t._1.sameElements(from.head)).take(size).keys.toSeq
  }

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    *
    * @return
    */
  override def chainScore(): BigInt = blocks.map(ib => score(ib._2)).sum

  override type NVCT = ElmBlockchain

  override def companion: NodeViewComponentCompanion = null

  override def score(block: ElmBlock): Score = BigInt("18446744073709551616") / block.baseTarget

  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  override def height(): Height = blocks.size

  override def heightOf(blockId: BlockId): Option[Height] =
    blockIds.find(_._2 sameElements blockId).map(_._1)

  override def discardBlock(): Try[ElmBlockchain] = ???

  override def blockAt(height: Height): Option[ElmBlock] =
    blockIds.get(height).flatMap(blocks.get)

  override def children(blockId: BlockId): Seq[ElmBlock] =
    heightOf(blockId).map(_ + 1).flatMap(blockAt).toSeq

  override def syncInfo: ElmSyncInfo = ElmSyncInfo(chainScore())

  override def compare(other: ElmSyncInfo): HistoryComparisonResult.Value = {
    val local = syncInfo.score
    val remote = other.score
    if (local < remote) HistoryComparisonResult.Older
    else if(local == remote) HistoryComparisonResult.Equal
    else HistoryComparisonResult.Younger
  }
}
