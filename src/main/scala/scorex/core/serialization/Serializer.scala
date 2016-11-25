package scorex.core.serialization

import scala.util.Try

trait Serializer[M] {
  def bytes(obj: M): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[M]
}
