package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.ActorRef

sealed trait ConnectionType

case object Incoming extends ConnectionType

case object Outgoing extends ConnectionType
