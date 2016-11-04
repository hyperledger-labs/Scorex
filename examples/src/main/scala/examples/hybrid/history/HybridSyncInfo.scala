package examples.hybrid.history

import examples.hybrid.blocks.{PosBlock, PowBlock}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class HybridSyncInfo(override val answer: Boolean,
                          bestPowBlockId: ModifierId,
                          bestPosBlockId: ModifierId
                         ) extends SyncInfo {
  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    Seq(PowBlock.ModifierTypeId -> bestPowBlockId,
      PosBlock.ModifierTypeId -> bestPosBlockId)

  override def bytes: Array[Byte] = ((if (answer) 1: Byte else 0: Byte) +: bestPowBlockId) ++ bestPosBlockId
}

object HybridSyncInfo {
  def parse(bytes: Array[Byte]): Try[HybridSyncInfo] = Try {
    val answer: Boolean = if (bytes.head == 1.toByte) true else false
    val lastPowBlockId = bytes.slice(1, 1 + NodeViewModifier.ModifierIdSize)
    val lastPosBlockId = bytes.slice(1 + NodeViewModifier.ModifierIdSize, 1 + 2 * NodeViewModifier.ModifierIdSize)
    HybridSyncInfo(answer, lastPowBlockId, lastPosBlockId)
  }
}

object HybridSyncInfoSpec extends SyncInfoSpec[HybridSyncInfo](HybridSyncInfo.parse)