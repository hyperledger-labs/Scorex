package scorex.core.serialization

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.bytes(this)

  def serializer: Serializer[M]
}
