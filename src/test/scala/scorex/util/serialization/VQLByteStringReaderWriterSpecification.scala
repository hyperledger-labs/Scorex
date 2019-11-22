package scorex.util.serialization

import akka.util.ByteString

class VQLByteStringReaderWriterSpecification extends VLQReaderWriterSpecification {

  override def byteBufReader(bytes: Array[Byte]): VLQReader = {
    new VLQByteStringReader(ByteString(bytes))
  }

  override def byteArrayWriter(): VLQWriter = {
    new VLQByteStringWriter()
  }
}
