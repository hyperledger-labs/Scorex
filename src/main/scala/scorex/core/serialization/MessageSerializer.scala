package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.crypto.hash.FastCryptographicHash._
import scorex.core.network.message.Message
import scorex.core.network.message.Message._

class MessageSerializer extends ScorexSerializer[Message[_]] {

  override def write(kryo: Kryo, output: Output, data: Message[_]): Unit = {
    val dataLength: Int = data.dataBytes.length
    output.writeBytes(MAGIC)
    output.writeByte(data.spec.messageCode)
    output.writeShort(dataLength)
    if (dataLength > 0) {
      val checksum = hash(data.dataBytes).take(ChecksumLength)
      output.writeBytes(checksum)
      output.writeBytes(data.dataBytes)
    }
  }

  override def read(kryo: Kryo, input: Input, c: Class[Message[_]]): Message[_] = {
    //TODO move MessageHandler.parseBytes() here.
/*
    val magic = input.readBytes(MagicLength)
    require(magic sameElements MAGIC)
    val code = input.readByte()
    val length = input.readInt()
    require(length >= 0)
    val msgData: Array[Byte] = length > 0 match {
      case true =>
        val checksum = input.readBytes(Message.ChecksumLength)

        //READ DATA
        val data = input.readBytes(length)

        //VALIDATE CHECKSUM
        val digest = hash(data).take(Message.ChecksumLength)

        //CHECK IF CHECKSUM MATCHES
        assert(checksum.sameElements(digest), s"Invalid data checksum length = $length")
        data

      case false => Array()
    }
*/

    ???
  }


}
