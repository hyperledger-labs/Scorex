package scorex.core.newserialization

import akka.util.ByteString

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {
  def serialize(obj: T, w: W): Unit
  def parse(r: R): TFamily
}

trait ScorexSerializer[T] extends Serializer[T, T, ScorexReader, ScorexWriter] {

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


