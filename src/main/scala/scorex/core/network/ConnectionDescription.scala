package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.ActorRef

case class ConnectionDescription(connection: ActorRef,
                                 direction: ConnectionType,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 remote: InetSocketAddress,
                                 localFeatures: Seq[PeerFeature])
