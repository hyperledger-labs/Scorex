package scorex.core.newserialization

trait Writer {
  type CH

  def putChunk(chunk: CH): this.type

  def put(x: Byte): this.type

  /** Encode integer as an unsigned byte asserting the range check
    * @param x integer value to encode
    * @return
    * @throws AssertionError if x is outside of the unsigned byte range
    */
  def putUByte(x: Int): this.type = {
    assert(x >= 0 && x <= 0xFF, s"$x is out of unsigned byte range")
    put(x.toByte)
  }
  def putBoolean(x: Boolean): this.type
  def putShort(x: Short): this.type

  /**
    * Encode Short that are positive
    *
    * Use [[putShort]] to encode values that might be negative.
    * @param x Short
    */
  def putUShort(x: Int): this.type

  /**
    * Encode signed Int.
    * Use [[putUInt]] to encode values that are positive.
    *
    * @param x Int
    */
  def putInt(x: Int): this.type

  /**
    * Encode Int that are positive.
    * Use [[putInt]] to encode values that might be negative.
    *
    * @param x Int
    */
  def putUInt(x: Long): this.type

  def putLong(x: Long): this.type

  def putBytes(xs: Array[Byte]): this.type

  /**
    * Encode an array of boolean values as a bit array
    *
    * @param xs array of boolean values
    */
  def putBits(xs: Array[Boolean]): this.type
}