package examples.tailchain.blockchain

import examples.commons.SimpleBoxTransaction
import examples.tailchain.modifiers.{TBlock, TModifier}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.BlockValidator
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging

import scala.util.Try

class TailChain(version: ModifierId,
                validators: Seq[BlockValidator[TBlock]])
  extends History[PublicKey25519Proposition,
    SimpleBoxTransaction,
    TModifier,
    TailChainSyncInfo,
    TailChain] with ScorexLogging {

  /**
    * Is there's no history, even genesis block
    */
  override def isEmpty: Boolean = ???

  override def modifierById(modifierId: ModifierId): Option[TModifier] = ???

  override def append(modifier: TModifier): Try[(TailChain, ProgressInfo[TModifier])] = ???

  override def drop(modifierId: ModifierId): TailChain = ???

  override def openSurfaceIds(): Seq[ModifierId] = ???

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  override def syncInfo(answer: Boolean): TailChainSyncInfo = ???

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: TailChainSyncInfo): HistoryComparisonResult.Value = ???

  override type NVCT = this.type
}
