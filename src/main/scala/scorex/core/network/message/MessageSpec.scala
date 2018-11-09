package scorex.core.network.message

import scorex.core.newserialization.{ScorexSerializer, Serializer}

trait MessageSpec[Content] extends ScorexSerializer[Content] {
  val messageCode: Message.MessageCode
  val messageName: String

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
