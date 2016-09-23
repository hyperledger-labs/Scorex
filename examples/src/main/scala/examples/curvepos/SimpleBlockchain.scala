package examples.curvepos

import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import scorex.core.NodeViewComponentCompanion
import scorex.core.consensus.{BlockChain, History}
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.collection.concurrent.TrieMap
import scala.util.Try


class SimpleBlockchain
  extends BlockChain[PublicKey25519Proposition, SimpleTransaction, SimpleBlock, SimpleBlockchain] {

  type Height = Int

  lazy val ids = IndexedSeq[BlockId]()
  lazy val blocks = TrieMap[BlockId, SimpleBlock]()

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = blocks.isEmpty

  override def blockById(blockId: BlockId): Option[SimpleBlock] = blocks.get(blockId)

  override def append(block: SimpleBlock): Try[(SimpleBlockchain, Option[RollbackTo[SimpleBlock]])] = {
    val blockId = block.id
    val parentId = block.parentId

    ???
  }

  //todo: should be ID | Seq[ID]
  override def openSurface(): Seq[BlockId] = Seq(ids.last)

  //todo: argument should be ID | Seq[ID]
  override def continuation(from: Seq[BlockId], size: Int): Seq[SimpleBlock] =
    continuationIds(from, size).map(blockById).map(_.get)

  //todo: argument should be ID | Seq[ID]
  override def continuationIds(from: Seq[BlockId], size: Int): Seq[BlockId] = {
    require(from.size == 1)
    ids.dropWhile(id => !id.sameElements(from.head)).take(size)
  }

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    *
    * @return
    */
  override def score(): BigInt = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = null

  override def score(block: SimpleBlock): Score = ???

  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  override def height(): Height = ???

  override def heightOf(blockId: BlockId): Option[Height] = ???

  override def discardBlock(): Try[SimpleBlockchain] = ???

  override def blockAt(height: Height): Option[SimpleBlock] = ???

  override def children(blockId: BlockId): Seq[SimpleBlock] = ???
}
