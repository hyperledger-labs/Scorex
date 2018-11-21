package scorex.core.network.message

import scorex.core.serialization.{ScorexMessageSerializer, ScorexSerializer}

trait MessageSpec[Content] extends ScorexMessageSerializer[Content] {
  val messageCode: Message.MessageCode
  val messageName: String

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
