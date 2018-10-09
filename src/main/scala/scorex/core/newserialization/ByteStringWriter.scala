package scorex.core.newserialization

import java.nio.ByteOrder
import java.util.{Arrays, BitSet}

import akka.util.ByteString

class ByteStringWriter() extends ScorexWriter {

  private implicit val byteOrder = ByteOrder.BIG_ENDIAN
  private val builder = ByteString.createBuilder

  override def put(x: Byte): this.type = {
    builder.putByte(x)
    this
  }

  override def putBoolean(x: Boolean): this.type = {
    val byte:Byte = if (x) 0x01 else 0x00
    builder.putByte(byte)
    this
  }

  override def putShort(x: Short): this.type = {
    builder.putShort(x)
    this
  }

  /**
    * Encode Short that are positive
    *
    * Use [[putShort]] to encode values that might be negative.
    *
    * @param x Short
    */
  override def putUShort(x: Int): this.type = {
    assert(x >= 0 && x <= 0xFFFF, s"$x is out of unsigned short range")
    builder.putShort(x.toShort)
    this
  }

  /**
    * Encode signed Int.
    * Use [[putUInt]] to encode values that are positive.
    *
    * @param x Int
    */
  override def putInt(x: Int): this.type = {
    builder.putInt(x)
    this
  }

  /**
    * Encode Int that are positive.
    * Use [[putInt]] to encode values that might be negative.
    * @param x Int
    */
  override def putUInt(x: Long): this.type = {
    builder.putInt(x.toInt)
    this
  }

  /**
    * Encode signed Long.
    * @param x Long
    */
  override def putLong(x: Long): this.type = {
    builder.putLong(x)
    this
  }

  override def putBytes(xs: Array[Byte]): this.type = {
    builder.putBytes(xs)
    this
  }

  /**
    * Encode an array of boolean values as a bit array
    *
    * @param xs array of boolean values
    */
  override def putBits(xs: Array[Boolean]): this.type = {
    if (xs.nonEmpty) {
      val bitSet = new BitSet(xs.length)
      xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool) }
      // pad the byte array to fix the "no bit was set" behaviour
      // see https://stackoverflow.com/questions/11209600/how-do-i-convert-a-bitset-initialized-with-false-in-a-byte-containing-0-in-java
      val bytes = Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)
      builder.putBytes(bytes)
    }
    this
  }


  /**
    * Encode String is shorter than 256 bytes
    * @param s String
    * @return
    */
  def putShortString(s: String): this.type = {
    val bytes = s.getBytes
    require(bytes.size < 256)
    putUByte(bytes.size.toByte)
    builder.putBytes(bytes)
    this
  }


  def result(): ByteString = {
    builder.result()
  }
}
