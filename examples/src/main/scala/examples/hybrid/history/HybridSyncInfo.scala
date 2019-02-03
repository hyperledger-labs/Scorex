package examples.hybrid.history

import examples.hybrid.blocks.{PosBlock, PowBlock}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * Stores up to 50 last PoW & Pos blocks
  * Thus maximum message size is about 100 * 32 ~= 3.2 KB
  * TODO answer is never used
  */
case class HybridSyncInfo(answer: Boolean,
                          lastPowBlockIds: Seq[ModifierId],
                          lastPosBlockId: ModifierId
                         ) extends SyncInfo {

  import HybridSyncInfo.MaxLastPowBlocks

  require(lastPowBlockIds.size <= MaxLastPowBlocks)

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    Seq(lastPowBlockIds.map(b => PowBlock.ModifierTypeId -> b) ++ Seq(PosBlock.ModifierTypeId -> lastPosBlockId)).flatten


  override type M = HybridSyncInfo

  override def serializer: ScorexSerializer[HybridSyncInfo] = HybridSyncInfoSerializer
}

object HybridSyncInfo {
  val MaxLastPowBlocks: Byte = 50 //don't make it more than 127 without changing serialization!
}

object HybridSyncInfoSerializer extends ScorexSerializer[HybridSyncInfo] {

  import HybridSyncInfo.MaxLastPowBlocks

  override def serialize(obj: HybridSyncInfo, w: Writer): Unit = {
    w.put(if (obj.answer) 1 else 0)
    w.put(obj.lastPowBlockIds.size.toByte)

    obj.lastPowBlockIds.foreach { b =>
      w.putBytes(idToBytes(b))
    }
    w.putBytes(idToBytes(obj.lastPosBlockId))
  }

  override def parse(r: Reader): HybridSyncInfo = {
    val answer: Boolean = r.getByte() == 1.toByte
    val lastPowBlockIdsSize = r.getByte()

    require(lastPowBlockIdsSize >= 0 && lastPowBlockIdsSize <= MaxLastPowBlocks)
    require(r.remaining == (lastPowBlockIdsSize + 1) * NodeViewModifier.ModifierIdSize)

    val lastPowBlockIds = (0 until lastPowBlockIdsSize).map { _ =>
      bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    }

    val lastPosBlockId = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))

    HybridSyncInfo(answer, lastPowBlockIds, lastPosBlockId)
  }
}

object HybridSyncInfoMessageSpec extends SyncInfoMessageSpec[HybridSyncInfo](HybridSyncInfoSerializer)
