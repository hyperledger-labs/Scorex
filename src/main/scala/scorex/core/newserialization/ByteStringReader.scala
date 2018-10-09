package scorex.core.newserialization

import java.nio.ByteOrder
import java.util.BitSet

import akka.util.ByteString

class ByteStringReader(byteString: ByteString) extends ScorexReader {

  private var it = byteString.iterator
  private var _position = 0
  private var _mark = 0
  private implicit val byteOrder = ByteOrder.BIG_ENDIAN

  /**
    * Get a byte at current position without advancing the position.
    *
    * @return byte at current position
    */
  override def peekByte(): Byte = byteString(position)

  override def getByte(): Byte = {
    incPosition
    it.getByte
  }

  override def getUByte(): Int = {
    incPosition
    it.getByte & 0xff
  }

  override def getShort(): Short = {
    incPosition
    it.getShort
  }

  /**
    * Decode positive Short.
    * Use '''only''' for values previously encoded with [[ByteStringWriter.putUShort]]
    *
    * @return signed Int
    */
  override def getUShort(): Int = {
    incPosition
    it.getShort & 0xffff
  }

  /**
    * Decode signed Int.
    * Use '''only''' for values previously encoded with [[ByteStringWriter.putInt]]
    *
    * @return signed Int
    */
  override def getInt(): Int = {
    incPosition
    it.getInt
  }

  /**
    * Decode positive Int.
    * Use '''only''' for values previously encoded with [[ByteStringWriter.putUInt]]
    *
    * @return signed Long
    */
  override def getUInt(): Long = {
    incPosition
    it.getInt & 0xffffffffL
  }

  /**
    * Decode signed Long.
    * Use '''only''' for values previously encoded with [[ByteStringWriter.putLong]]
    *
    * @return signed Long
    */
  override def getLong(): Long = {
    incPosition
    it.getLong
  }

  /**
    * Decode positive Long.
    * Use '''only''' for values previously encoded with [[ByteStringWriter.putULong]]
    *
    * @return signed Long
    */
  override def getULong(): Long = ???

  override def getBytes(size: Int): Array[Byte] = {
    incPosition(size)
    it.getBytes(size)
  }

  /**
    * Decode array of boolean values previously encode with [[ByteStringWriter.putBits]]
    *
    * @param size expected size of decoded array
    * @return decoded array of boolean values
    */
  override def getBits(size: Int): Array[Boolean] = {
    if (size == 0) {
      Array[Boolean]()
    } else {
      val bytesSize = (size + 7) / 8
      incPosition(bytesSize)
      val bitSet = BitSet.valueOf(it.getBytes(bytesSize))
      val boolArray = new Array[Boolean](size)
      var i = 0
      while (i < size) {
        boolArray(i) = bitSet.get(i)
        i += 1
      }
      boolArray
    }
  }

  override def getOption[T](getValue: => T): Option[T] = ???

  override def mark(): Reader = {
    _mark = _position
    this
  }

  override def consumed: Int = _position - _mark

  override def position: Int = _position

  override def position_=(p: Int): Unit = {
    _position = p
    it = byteString.iterator.drop(p)
  }

  override def remaining: Int = it.len - _position

  private var lvl: Int = 0
  override def level: Int = lvl

  override def level_=(v: Int): Unit = {
    lvl = v
  }

  /**
    * Decode String is shorter than 256 bytes
 *
    * @return
    */
  def getShortString(): String = {
    val size = getUByte()
    new String(getBytes(size))
  }

  @inline
  private def incPosition = {
    _position += 1
  }

  @inline
  private def incPosition(size: Int) = {
    _position += size
  }
}
