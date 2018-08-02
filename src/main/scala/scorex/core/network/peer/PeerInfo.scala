package scorex.core.network.peer

import scorex.core.network.{ConnectionType, PeerFeature}

case class PeerInfo(lastSeen: Long,
                    nodeName: Option[String] = None,
                    connectionType: Option[ConnectionType] = None,
                    features: Seq[PeerFeature] = Seq())
