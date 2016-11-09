package scorex.core.serialization

import com.esotericsoftware.kryo.Serializer

abstract class ScorexSerializer[T] extends Serializer[T] {
  override def getAcceptsNull: Boolean = true
}
