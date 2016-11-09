package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import examples.curvepos.SimpleSyncInfo
import scorex.core.NodeViewModifier
import scorex.core.serialization.ScorexSerializer

class SimpleSyncInfoSerializer extends ScorexSerializer[SimpleSyncInfo] {
  override def write(kryo: Kryo, output: Output, data: SimpleSyncInfo): Unit = {
    if (data.answer) output.writeByte(1: Byte) else output.writeByte(0: Byte)
    output.writeBytes(data.lastBlockID)
    output.writeBytes(data.score.toByteArray)
  }

  override def read(kryo: Kryo, input: Input, c: Class[SimpleSyncInfo]): SimpleSyncInfo = {
    val answer = if (input.readByte() == 1) true else false
    val mid = input.readBytes(NodeViewModifier.ModifierIdSize)
    val scoreBytes = input.readBytes(input.limit() - input.position())
    SimpleSyncInfo(answer, mid, BigInt(scoreBytes))
  }
}
