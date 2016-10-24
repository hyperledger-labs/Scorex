package examples.curvepos

import examples.curvepos.transaction.SimpleBlock
import scorex.core.NodeViewModifier
import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class SimpleSyncInfo(answer: Boolean, lastBlockID: NodeViewModifier.ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    Seq(SimpleBlock.ModifierTypeId -> lastBlockID)
  }

  override def bytes: Array[Byte] = (if (answer) 1: Byte else 0: Byte) +: (lastBlockID ++ score.toByteArray)
}

object SimpleSyncInfo {
  def parse(bytes: Array[Byte]): Try[SimpleSyncInfo] = Try {
    val answer = if (bytes.head == 1) true else if (bytes.head == 0) false else throw new Exception("wrong answer byte")
    val mid = bytes.tail.take(NodeViewModifier.ModifierIdSize)
    val scoreBytes = bytes.tail.drop(NodeViewModifier.ModifierIdSize)
    SimpleSyncInfo(answer, mid, BigInt(scoreBytes))
  }
}

object SimpleSyncInfoSpec extends SyncInfoSpec[SimpleSyncInfo](SimpleSyncInfo.parse)
