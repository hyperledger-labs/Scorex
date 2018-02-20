package scorex.core.app

import scorex.core.serialization.{BytesSerializable, Serializer}
import scala.util.Try

object Version {
  def apply(v: String): Version = {
    val splitted = v.split("\\.")
    Version(splitted(0).toByte, splitted(1).toByte, splitted(2).toByte)
  }
}
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte) extends BytesSerializable {
  override type M = Version

  override def serializer: Serializer[Version] = ApplicationVersionSerializer
}

object ApplicationVersionSerializer extends Serializer[Version] {
  val SerializedVersionLength = 3

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
