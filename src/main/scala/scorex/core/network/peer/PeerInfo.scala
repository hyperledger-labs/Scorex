package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.network.PeerFeature

case class PeerInfo(lastSeen: Long,
                    declaredAddress: Option[InetSocketAddress],
                    nodeName: Option[String] = None,
                    connectionType: Option[ConnectionType] = None,
                    features: Seq[PeerFeature] = Seq()) {

  val localAddress: Option[InetSocketAddress] = features.collectFirst {
    case LocalAddressPeerFeature(address) => address
  }

  def isReachable: Boolean = declaredAddress.isDefined || localAddress.isDefined

}
