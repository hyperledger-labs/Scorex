package scorex.core.network.message

import com.google.common.primitives.{Bytes, Ints}
import scorex.core.crypto.hash.FastCryptographicHash._
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.{Success, Try}

case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Array[Byte], Content],
                            source: Option[ConnectedPeer]) extends BytesSerializable {

  import Message.{ChecksumLength, MAGIC}

  lazy val dataBytes = input match {
    case Left(db) => db
    case Right(d) => spec.serializeData(d)
  }

  lazy val data: Try[Content] = input match {
    case Left(db) => spec.deserializeData(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  override type M = Message[Content]

  override def serializer: Serializer[Message[Content]] = new MessageSerializer[Content]
}

class MessageSerializer[Content] extends Serializer[Message[Content]] {
  import Message.{ChecksumLength, MAGIC}

  override def bytes(obj: Message[Content]): Array[Byte] = {
    val dataWithChecksum = if (obj.dataLength > 0) {
      val checksum = hash(obj.dataBytes).take(ChecksumLength)
      Bytes.concat(checksum, obj.dataBytes)
    } else obj.dataBytes //empty array

    MAGIC ++ Array(obj.spec.messageCode) ++ Ints.toByteArray(obj.dataLength) ++ dataWithChecksum
  }

  //TODO how?
  override def parseBytes(bytes: Array[Byte]): Try[Message[Content]] = ???
}

object Message {
  type MessageCode = Byte

  val MAGIC = Array(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MagicLength = MAGIC.length

  val ChecksumLength = 4
}
