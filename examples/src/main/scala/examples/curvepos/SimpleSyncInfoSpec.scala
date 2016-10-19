package examples.curvepos

import examples.curvepos.transaction.SimpleBlock
import scorex.core.NodeViewModifier
import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class SimpleSyncInfo(lastBlockID: NodeViewModifier.ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    Seq(SimpleBlock.ModifierTypeId -> lastBlockID)
  }

  override def bytes: Array[Byte] = lastBlockID ++ score.toByteArray
}

object SimpleSyncInfo {
  def parse(bytes: Array[Byte]): Try[SimpleSyncInfo] = Try {
    val mid = bytes.take(NodeViewModifier.ModifierIdSize)
    val scoreBytes = bytes.drop(NodeViewModifier.ModifierIdSize)
    SimpleSyncInfo(mid, BigInt(scoreBytes))
  }
}

object SimpleSyncInfoSpec extends SyncInfoSpec[SimpleSyncInfo](SimpleSyncInfo.parse)
