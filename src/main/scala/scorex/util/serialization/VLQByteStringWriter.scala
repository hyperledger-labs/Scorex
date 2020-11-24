package scorex.util.serialization

import java.nio.ByteOrder

import akka.util.ByteString

class VLQByteStringWriter extends VLQWriter {
  override type CH = ByteString
  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  @inline
  override def newWriter(): Writer.Aux[CH] = {
    new VLQByteStringWriter()
  }

  private val builder = ByteString.createBuilder

  @inline
  override def length(): Int = builder.length

  @inline
  override def putChunk(byteString: ByteString): this.type = {
    builder.append(byteString)
    this
  }

  @inline
  override def put(x: Byte): this.type = {
    builder.putByte(x)
    this
  }

  @inline
  override def putBoolean(x: Boolean): this.type = {
    val byte: Byte = if (x) 0x01 else 0x00
    builder.putByte(byte)
    this
  }

  override def putBytes(xs: Array[Byte],
                        offset: Int,
                        length: Int): VLQByteStringWriter.this.type = {
    builder.putBytes(xs, offset, length)
    this
  }

  @inline
  override def putBytes(xs: Array[Byte]): this.type = {
    builder.putBytes(xs)
    this
  }

  @inline
  override def result(): ByteString = {
    builder.result()
  }

  @inline override def toBytes: Array[Byte] = {
    builder.result().toArray
  }
}
