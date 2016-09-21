package examples.curvepos

import examples.curvepos.transaction.{SimpleBlock, SimplestTransaction}
import scorex.core.NodeViewComponentCompanion
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scala.util.Try


class Blockchain
  extends History[PublicKey25519Proposition, SimplestTransaction, SimpleBlock, Blockchain] {
  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = ???

  override def blockById(blockId: BlockId): Option[SimpleBlock] = ???

  override def append(block: SimpleBlock): Try[(Blockchain, Option[RollbackTo[SimpleBlock]])] = ???

  //todo: should be ID | Seq[ID]
  override def openSurface(): Seq[BlockId] = ???

  override def continuation(from: Seq[BlockId], size: Int): Seq[SimpleBlock] = ???

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
