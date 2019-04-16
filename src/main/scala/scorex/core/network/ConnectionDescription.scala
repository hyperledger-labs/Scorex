package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.ActorRef

case class ConnectionDescription(connection: ActorRef,
                                 connectionId: ConnectionId,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 localFeatures: Seq[PeerFeature])
