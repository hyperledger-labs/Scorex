package scorex.core.network

sealed trait ConnectionType

case object Incoming extends ConnectionType

case object Outgoing extends ConnectionType
