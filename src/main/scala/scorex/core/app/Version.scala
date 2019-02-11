package scorex.core.app

import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

/**
  * Version of p2p protocol. Node can only process messages of it's version or lower.
  */
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte) extends BytesSerializable {
  override type M = Version

  override def serializer: Serializer[Version] = ApplicationVersionSerializer
}

object Version {
  def apply(v: String): Version = {
    val splitted = v.split("\\.")
    Version(splitted(0).toByte, splitted(1).toByte, splitted(2).toByte)
  }

  val initial: Version = Version(0, 0, 1)
  val last: Version = Version(0, 0, 1)
}

object ApplicationVersionSerializer extends Serializer[Version] {
  val SerializedVersionLength: Int = 3

  override def toBytes(obj: Version): Array[Byte] =
    Array(obj.firstDigit, obj.secondDigit, obj.thirdDigit)

  override def parseBytes(bytes: Array[Byte]): Try[Version] = Try {
    Version(
      bytes(0),
      bytes(1),
      bytes(2)
    )
  }
}