package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.NodeViewModifier
import scorex.core.network.message.BasicMsgDataTypes.InvData

class InvDataSerializer extends ScorexSerializer[InvData] {
  //todo: fetch from settings file?
  val MaxObjects = 500

  override def write(kryo: Kryo, output: Output, data: InvData): Unit = {
    output.writeByte(data.typeId)
    output.writeShort(data.modifierIds.length)
    data.modifierIds.foreach(m => output.writeBytes(m))
  }

  override def read(kryo: Kryo, input: Input, c: Class[InvData]): InvData = {
    val id = input.readByte()
    val length = input.readShort()
    require(length > 0)
    require(length <= MaxObjects)
    val modifiers = (0 until length) map (i => input.readBytes(NodeViewModifier.ModifierIdSize))
    InvData(id, modifiers)
  }


}
