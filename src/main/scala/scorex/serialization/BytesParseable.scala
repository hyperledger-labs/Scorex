package scorex.serialization

import com.google.common.primitives.Ints

import scala.annotation.tailrec
import scala.util.Try

/**
  * Interface for objects able to deserialize bytes to an instance of T
  */
trait BytesParseable[T] {
  def parseBytes(bytes: Array[Byte]): Try[T]

  protected def parseArraySizes(bytes: Array[Byte]): Seq[Array[Byte]] = {
    @tailrec
    def loop(b: Array[Byte], acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      val length = Ints.fromByteArray(b.take(4))
      val objBytes = b.slice(4, 4 + length)
      val restBytes = b.slice(4 + length, b.length)
      if (restBytes.isEmpty) objBytes +: acc
      else loop(restBytes, objBytes +: acc)
    }
    loop(bytes, Seq()).reverse
  }
}
