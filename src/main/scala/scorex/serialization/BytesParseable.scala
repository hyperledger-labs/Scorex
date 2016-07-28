package scorex.serialization

import scala.util.Try

/**
  * Interface for objects able to deserialize bytes to an instance of T
  */
trait BytesParseable[T] {
  def parseBytes(bytes: Array[Byte]): Try[T]
}
