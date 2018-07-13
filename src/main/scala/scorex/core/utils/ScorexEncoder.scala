package scorex.core.utils

import scorex.crypto.encode.{Base16, BytesEncoder}

import scala.util.Try

trait ScorexEncoder extends BytesEncoder {
  @inline
  override val Alphabet: String = Base16.Alphabet

  @inline
  override def encode(input: Array[Byte]): String = Base16.encode(input)

  @inline
  override def decode(input: String): Try[Array[Byte]] = Base16.decode(input)

  @inline
  def encode(input: String): String = input

}

object ScorexEncoderImpl extends ScorexEncoder
