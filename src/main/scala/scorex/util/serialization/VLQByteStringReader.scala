package scorex.util.serialization

import java.nio.ByteOrder

import akka.util.ByteString

class VLQByteStringReader(byteString: ByteString) extends VLQReader {

  type CH = ByteString

  private var it = byteString.iterator
  private var _position = 0
  private var _mark = 0
  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  @inline
  override def newReader(chunk: ByteString): Reader.Aux[CH] = {
    new VLQByteStringReader(chunk)
  }

  /**
    * Get a byte at current position without advancing the position.
    *
    * @return byte at current position
    */
  @inline
  override def peekByte(): Byte = byteString(position)

  @inline
  override def getByte(): Byte = {
    incPosition()
    it.getByte
  }

  @inline
  override def getBytes(size: Int): Array[Byte] = {
    require(size <= remaining, s"Not enough bytes in the buffer: $size")
    incPosition(size)
    it.getBytes(size)
  }

  @inline
  override def getChunk(size: Int): ByteString = {
    it.getByteString(size)
  }

  @inline
  override def mark(): this.type = {
    _mark = _position
    this
  }

  @inline
  override def consumed: Int = _position - _mark

  @inline
  override def position: Int = _position

  @inline
  override def position_=(p: Int): Unit = {
    _position = p
    it = byteString.iterator.drop(p)
  }

  @inline
  override def remaining: Int = it.len

  @inline
  private def incPosition(): Unit = {
    _position += 1
  }

  @inline
  private def incPosition(size: Int): Unit = {
    _position += size
  }
}
