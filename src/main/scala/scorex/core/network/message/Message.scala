package scorex.core.network.message

import scorex.core.network.ConnectedPeer

case class Message[Content](spec: MessageSpec[Content],
                            content: Content,
                            source: Option[ConnectedPeer])

object Message {
  type MessageCode = Byte
  val MAGIC: Array[Byte] = Array[Byte](0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)
}
