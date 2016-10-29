package examples.hybrid.history

import examples.curvepos.SimpleSyncInfo
import examples.hybrid.blocks.{PosBlock, PowBlock}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class HybridSyncInfo(override val answer: Boolean,
                          lastPowBlockId: ModifierId,
                          lastPosBlockId: ModifierId
                         ) extends SyncInfo {
  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    Seq(PowBlock.ModifierTypeId -> lastPowBlockId,
        PosBlock.ModifierTypeId -> lastPosBlockId)

  override def bytes: Array[Byte] =
    (PowBlock.ModifierTypeId +: lastPowBlockId) ++
      (PosBlock.ModifierTypeId +: lastPosBlockId)
}

object HybridSyncInfo{
  //todo: for Dmitry
  def parse(bytes: Array[Byte]): Try[HybridSyncInfo] = ???
}

object HybridSyncInfoSpec extends SyncInfoSpec[HybridSyncInfo](HybridSyncInfo.parse)