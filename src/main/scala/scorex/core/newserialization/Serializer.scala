package scorex.core.newserialization

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {
  def parse(r: R): TFamily
  def serialize(obj: T, w: W): Unit
}

trait ScorexSerializer[T] extends Serializer[T, T, ScorexReader, ScorexWriter] {

}