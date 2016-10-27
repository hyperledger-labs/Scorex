package examples.hybrid.history

import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.NodeViewComponentCompanion
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

class HybridHistory
  extends History[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HybridSyncInfo, HybridHistory]{

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = ???

  override def blockById(blockId: BlockId): Option[HybridPersistentNodeViewModifier] = ???

  override def append(block: HybridPersistentNodeViewModifier): Try[(HybridHistory, Option[RollbackTo[HybridPersistentNodeViewModifier]])] = ???

  //todo: is it needed?
  override def openSurfaceIds(): Seq[BlockId] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[HybridPersistentNodeViewModifier]] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): HybridSyncInfo = ???

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
