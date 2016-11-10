package scorex.core.network.message

import scorex.core.serialization.ScorexKryoPool

trait MessageSpec[Content] {
  val c: Class[Content]

  val messageCode: Message.MessageCode
  val messageName: String


  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
