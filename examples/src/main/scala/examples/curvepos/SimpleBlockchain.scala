package examples.curvepos

import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import scorex.core.NodeViewComponentCompanion
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.NodeViewModifier
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.collection.concurrent.TrieMap
import scala.util.Try


class SimpleBlockchain
  extends History[PublicKey25519Proposition, SimpleTransaction, SimpleBlock, SimpleBlockchain] {

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

  override def append(block: SimpleBlock): Try[(SimpleBlockchain, Option[RollbackTo[SimpleBlock]])] = ???

  //todo: should be ID | Seq[ID]
  override def openSurface(): Seq[BlockId] = Seq(ids.last)

  //todo: argument should be ID | Seq[ID]
  override def continuation(from: Seq[BlockId], size: Int): Seq[SimpleBlock] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuationIds(from: Seq[BlockId], size: Int): Seq[BlockId] = ???

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    *
    * @return
    */
  override def score(): BigInt = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = null
}
