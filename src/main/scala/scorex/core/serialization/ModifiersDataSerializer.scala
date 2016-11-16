package scorex.core.serialization

import java.nio.ByteBuffer

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.NodeViewModifier
import scorex.core.network.message.BasicMsgDataTypes.ModifiersData

class ModifiersDataSerializer extends ScorexSerializer[ModifiersData] {
  val MaxObjects = 500

  override def write(kryo: Kryo, output: Output, data: ModifiersData): Unit = {
    output.writeByte(data.typeId)
    output.writeShort(data.modifiers.size)
    data.modifiers.toSeq.sortBy(e => ByteBuffer.wrap(e._1)).foreach { m =>
      output.writeBytes(m._1)
      output.writeShort(m._2.length)
      output.writeBytes(m._2)
    }
  }

  override def read(kryo: Kryo, input: Input, c: Class[ModifiersData]): ModifiersData = {
    val typeId = input.readByte()
    val length = input.readShort()
    require(length > 0)
    require(length <= MaxObjects)
    val modifiers = (0 until length) map { i =>
      val id = input.readBytes(NodeViewModifier.ModifierIdSize)
      val l = input.readShort()
      val mod = input.readBytes(l)
      id -> mod
    }
    ModifiersData(typeId, modifiers.toMap)
  }


}
