package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.network.{ConnectionType, PeerFeature}

case class PeerInfo(lastSeen: Long,
                    declaredAddress: Option[InetSocketAddress],
                    nodeName: Option[String] = None,
                    connectionType: Option[ConnectionType] = None,
                    features: Seq[PeerFeature] = Seq()) {

  def reachablePeer: Boolean = {
    declaredAddress.isDefined || localAddress.isDefined
  }

  def localAddress: Option[InetSocketAddress] = {
    features.collectFirst { case LocalAddressPeerFeature(addr) => addr }
  }
}
