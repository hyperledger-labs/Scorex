package scorex.core.network

sealed trait ConnectionDirection {
  val isIncoming: Boolean
  val isOutgoing: Boolean = !isIncoming
}

case object Incoming extends ConnectionDirection {
  override val isIncoming: Boolean = true
  override val isOutgoing: Boolean = false
}

case object Outgoing extends ConnectionDirection {
  override val isIncoming: Boolean = false
  override val isOutgoing: Boolean = true
}
