package scorex.core.network

sealed trait ConnectionDirection {
  def isIncoming: Boolean
  def isOutgoing: Boolean = !isIncoming
}

case object Incoming extends ConnectionDirection {
  override val isIncoming: Boolean = true
}

case object Outgoing extends ConnectionDirection {
  override val isIncoming: Boolean = false
}
