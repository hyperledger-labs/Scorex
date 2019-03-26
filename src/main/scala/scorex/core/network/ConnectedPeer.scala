package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import scorex.core.network.peer.PeerInfo

/**
  * Peer connected to our node
  *
  * @param remoteAddress     - connection address
  * @param handlerRef - reference to PeerConnectionHandler that is responsible for communication with this peer
  * @param peerInfo   - information about this peer. May be None if peer is connected, but is not handshaked yet
  */
case class ConnectedPeer(remoteAddress: InetSocketAddress,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  override def hashCode(): Int = remoteAddress.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case p: ConnectedPeer => p.remoteAddress == this.remoteAddress && peerInfo == this.peerInfo
    case _ => false
  }

  override def toString: String = s"ConnectedPeer($remoteAddress)"
}
