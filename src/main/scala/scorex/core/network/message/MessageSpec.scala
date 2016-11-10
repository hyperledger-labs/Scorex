package scorex.core.network.message

trait MessageSpec[Content] {
  val c: Class[Content]

  val messageCode: Message.MessageCode
  val messageName: String


  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
