package scorex.core.serialization

import akka.util.ByteString
import scorex.util.serialization._

trait ScorexSerializer[T] extends Serializer[T, T, Reader, Writer] {

  def serialize(obj: T): ByteString = {
    val writer = new ByteStringWriter()
    serialize(obj, writer)
    writer.result()
  }

  def parse(byteString: ByteString): T = {
    val reader = new ByteStringReader(byteString)
    parse(reader)
  }

  // TODO implement BytesReader/Writer and use them
  def toBytes(obj: T): Array[Byte] = {
    serialize(obj).toArray
  }

  def parseBytes(bytes: Array[Byte]): T = {
    parse(ByteString(bytes))
  }
}

trait ScorexMessageSerializer[T] extends Serializer[T, T, ByteStringReader, ByteStringWriter] {

  def serialize(obj: T): ByteString = {
    val writer = new ByteStringWriter()
    serialize(obj, writer)
    writer.result()
  }

  def parse(byteString: ByteString): T = {
    val reader = new ByteStringReader(byteString)
    parse(reader)
  }
}
