package scorex.core.serialization

// import akka.util.ByteString

import scala.util.Try

trait Serializer[M] {
  def toBytes(obj: M): Seq[Byte]

  def parseBytes(bytes: Seq[Byte]): Try[M]
}
