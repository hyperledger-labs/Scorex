package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.network.{ConnectionType, PeerFeature}

case class PeerInfo(lastSeen: Long,
                    decalerdAddress: InetSocketAddress,
                    nodeName: Option[String] = None,
                    connectionType: Option[ConnectionType] = None,
                    features: Seq[PeerFeature] = Seq())
