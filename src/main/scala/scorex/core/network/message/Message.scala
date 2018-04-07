package scorex.core.network.message

import com.google.common.primitives.Ints
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.hash.Blake2b256

import scala.util.{Success, Try}
import akka.util.ByteString

case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Seq[Byte], Content],
                            source: Option[ConnectedPeer]) extends BytesSerializable {

  lazy val dataBytes = input match {
    case Left(db) => db
    case Right(d) => spec.toBytes(d)
  }

  lazy val data: Try[Content] = input match {
    case Left(db) => spec.parseBytes(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  override type M = Message[Content]

  override def serializer: Serializer[Message[Content]] = new MessageSerializer[Content]
}

class MessageSerializer[Content] extends Serializer[Message[Content]] {

  import Message.{ChecksumLength, MAGIC}

  override def toBytes(obj: Message[Content]): Seq[Byte] = {
    val dataWithChecksum = if (obj.dataLength > 0) {
      val checksum = Blake2b256.hash(obj.dataBytes.toArray).take(ChecksumLength)
      ByteString(checksum) ++ obj.dataBytes
    } else obj.dataBytes // empty

    MAGIC ++
      ByteString(obj.spec.messageCode) ++
      ByteString(Ints.toByteArray(obj.dataLength)) ++
      dataWithChecksum
  }

  //TODO move MessageHandler.parseBytes here
  override def parseBytes(bytes: Seq[Byte]): Try[Message[Content]] = ???
}

object Message {
  type MessageCode = Byte

  val MAGIC = ByteString(0x12: Byte, 0x34: Byte, 0x56: Byte, 0x78: Byte)

  val MagicLength = MAGIC.length

  val ChecksumLength = 4
}
