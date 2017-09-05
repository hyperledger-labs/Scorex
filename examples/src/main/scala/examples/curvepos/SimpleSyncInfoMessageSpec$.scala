package examples.curvepos

import examples.curvepos.transaction.SimpleBlock
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.serialization.Serializer
import scorex.core.network.message.SyncInfoMessageSpec

import scala.util.Try

case class SimpleSyncInfo(answer: Boolean, lastBlockID: ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    Seq(SimpleBlock.ModifierTypeId -> lastBlockID)
  }

  override type M = SimpleSyncInfo

  override def serializer: Serializer[SimpleSyncInfo] = SimpleSyncInfoSerializer
}


object SimpleSyncInfoSerializer extends Serializer[SimpleSyncInfo] {

  override def toBytes(obj: SimpleSyncInfo): Array[Byte] =
    (if (obj.answer) 1: Byte else 0: Byte) +: (obj.lastBlockID ++ obj.score.toByteArray)

  def parseBytes(bytes: Array[Byte]): Try[SimpleSyncInfo] = Try {
    val answer = if (bytes.head == 1) true else if (bytes.head == 0) false else throw new Exception("wrong answer byte")
    val mid = ModifierId @@ bytes.tail.take(NodeViewModifier.ModifierIdSize)
    val scoreBytes = bytes.tail.drop(NodeViewModifier.ModifierIdSize)
    SimpleSyncInfo(answer, mid, BigInt(scoreBytes))
  }
}

object SimpleSyncInfoMessageSpec extends SyncInfoMessageSpec[SimpleSyncInfo](SimpleSyncInfoSerializer.parseBytes)
