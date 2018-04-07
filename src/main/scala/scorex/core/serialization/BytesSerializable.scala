package scorex.core.serialization

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Seq[Byte] = serializer.toBytes(this)

  def serializer: Serializer[M]
}
