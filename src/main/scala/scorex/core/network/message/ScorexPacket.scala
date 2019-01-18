package scorex.core.network.message

import java.nio.ByteOrder

import akka.util.ByteString
import scorex.core.network.ConnectedPeer
import scorex.crypto.hash.Blake2b256

case class ScorexPacket(messageCode: Message.MessageCode,
                        payload: ByteString,
                        source: Option[ConnectedPeer]) {
  import ScorexPacket._

  def length: Int = payload.length + HeaderLength + ChecksumLength
}

object ScorexPacket {
  val MagicLength: Int = Message.MAGIC.length
  val ChecksumLength: Int = 4
  val HeaderLength: Int = MagicLength + 5

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  def serialize(packet: ScorexPacket): ByteString = {

    val builder = ByteString.createBuilder
      .putBytes(Message.MAGIC)
      .putByte(packet.messageCode)
      .putInt(packet.payload.length)

    if (packet.payload.length > 0) {
      val checksum = Blake2b256.hash(packet.payload.toArray).take(ChecksumLength)
      builder.putBytes(checksum).append(packet.payload)
    }
    builder.result()
  }

  //MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  def deserialize(byteString: ByteString, sourceOpt: Option[ConnectedPeer]): Option[ScorexPacket] = {
    if (byteString.length < HeaderLength) {
      None
    } else {
      val it = byteString.iterator
      val magic = it.getBytes(MagicLength)
      val msgCode = it.getByte
      val length = it.getInt
      require(java.util.Arrays.equals(magic, Message.MAGIC), "Wrong magic bytes" + magic.mkString)
      require(length >= 0, "Data length is negative!")

      if (length != 0 && byteString.length < length + HeaderLength + ChecksumLength) {
        None
      } else {
        val msgData = if (length > 0) {
          val checksum = it.getBytes(ChecksumLength)
          val data = it.getByteString(length)
          val digest = Blake2b256.hash(data.toArray).take(ChecksumLength)
          if (!java.util.Arrays.equals(checksum, digest)) throw new Error(s"Invalid data checksum length = $length")
          data
        } else {
          ByteString.empty
        }

        Some(ScorexPacket(msgCode, msgData, sourceOpt))
      }
    }
  }
}
