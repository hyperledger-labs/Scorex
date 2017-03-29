package examples.tailchain.blockchain

import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HistoryStorage, HybridSyncInfo}
import examples.hybrid.mining.MiningConstants
import examples.tailchain.modifiers.TModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.BlockValidator
import scorex.core.consensus.History
import scorex.core.consensus.History.{ModifierIds, ProgressInfo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging

import scala.util.Try

class TailChain(storage: HistoryStorage,
                    settings: MiningConstants,
                    validators: Seq[BlockValidator[HybridBlock]])
  extends History[PublicKey25519Proposition,
    SimpleBoxTransaction,
    TModifier,
    HybridSyncInfo,
    TailChain] with ScorexLogging {
  /**
    * Is there's no history, even genesis block
    */
  override def isEmpty: Boolean = ???

  override def modifierById(modifierId: ModifierId): Option[TModifier] = ???

  override def append(modifier: TModifier): Try[(TailChain, ProgressInfo[TModifier])] = ???

  override def drop(modifierId: ModifierId): TailChain = ???

  //todo: output should be ID | Seq[ID]
  override def openSurfaceIds(): Seq[ModifierId] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  override def syncInfo(answer: Boolean): HybridSyncInfo = ???

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type
}
