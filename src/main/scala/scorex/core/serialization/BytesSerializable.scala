package scorex.core.serialization

trait BytesSerializable extends Serializable {

  def bytes: Array[Byte]
}
