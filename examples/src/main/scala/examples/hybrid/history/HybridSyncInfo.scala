package examples.hybrid.history

import examples.hybrid.blocks.{PosBlock, PowBlock}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo

//todo: add score
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