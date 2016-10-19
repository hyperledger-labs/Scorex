package examples.curvepos

import examples.curvepos.SimpleBlockchain.Height
import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import scorex.core.NodeViewComponentCompanion
import scorex.core.consensus.History.{BlockId, HistoryComparisonResult, RollbackTo}
import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class SimpleBlockchain(blockIds: Map[Height, BlockId] = Map(), blocks: Map[BlockId, SimpleBlock] = Map())
  extends BlockChain[PublicKey25519Proposition, SimpleTransaction, SimpleBlock, SimpleBlockchain] {

  import BlockChain.Score

  override type SI = SimpleSyncInfo

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[SimpleBlock] =
    blocks.find(_._1.sameElements(blockId)).map(_._2)

  override def append(block: SimpleBlock): Try[(SimpleBlockchain, Option[RollbackTo[SimpleBlock]])] = synchronized {
    val blockId = block.id
    val parentId = block.parentId

    if (blockIds.isEmpty || (lastBlock.id sameElements parentId)) {
      val h = height() + 1
      val newChain = SimpleBlockchain(blockIds + (h -> blockId), blocks + (blockId -> block))
      Success(newChain, None)
    } else Failure(new Exception(s"Last block id is ${Base58.encode(blockIds.last._2)}, " +
      s"expected ${Base58.encode(parentId)}}"))
  }

  //todo: should be ID | Seq[ID]
  override def openSurface(): Seq[BlockId] = Seq(blocks.last._1)

  //todo: argument should be ID | Seq[ID]
  override def continuation(from: Seq[BlockId], size: Int): Seq[SimpleBlock] =
    continuationIds(from, size).map(blockById).map(_.get)

  //todo: argument should be ID | Seq[ID]
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

  override type NVCT = SimpleBlockchain

  override def companion: NodeViewComponentCompanion = null

  override def score(block: SimpleBlock): Score = BigInt("18446744073709551616") / block.baseTarget

  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  override def height(): Height = blocks.size

  override def heightOf(blockId: BlockId): Option[Height] =
    blockIds.find(_._2 sameElements blockId).map(_._1)

  override def discardBlock(): Try[SimpleBlockchain] = ???

  override def blockAt(height: Height): Option[SimpleBlock] =
    blockIds.get(height).flatMap(blocks.get)

  override def children(blockId: BlockId): Seq[SimpleBlock] =
    heightOf(blockId).map(_ + 1).flatMap(blockAt).toSeq

  override def syncInfo: SimpleSyncInfo = SimpleSyncInfo(chainScore())

  override def compare(other: SimpleSyncInfo): HistoryComparisonResult.Value = {
    val local = syncInfo.score
    val remote = other.score
    if (local < remote) HistoryComparisonResult.Older
    else if(local == remote) HistoryComparisonResult.Equal
    else HistoryComparisonResult.Younger
  }
}

object SimpleBlockchain {
  type Height = Int
}
