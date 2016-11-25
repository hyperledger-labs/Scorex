package scorex.core.app

import com.google.common.primitives.Ints
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class ApplicationVersion(firstDigit: Int, secondDigit: Int, thirdDigit: Int) extends BytesSerializable {
  override type M = ApplicationVersion

  override def serializer: Serializer[ApplicationVersion] = ApplicationVersionSerializer
}

object ApplicationVersionSerializer extends Serializer[ApplicationVersion] {
  val SerializedVersionLength = 12

  override def bytes(obj: ApplicationVersion): Array[Byte] =
    Ints.toByteArray(obj.firstDigit) ++ Ints.toByteArray(obj.secondDigit) ++ Ints.toByteArray(obj.thirdDigit)

  override def parseBytes(bytes: Array[Byte]): Try[ApplicationVersion] = Try {
    ApplicationVersion(
      Ints.fromByteArray(bytes.slice(0, 4)),
      Ints.fromByteArray(bytes.slice(4, 8)),
      Ints.fromByteArray(bytes.slice(8, 12))
    )
  }
}