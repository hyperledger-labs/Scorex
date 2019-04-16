package scorex.core.network

import akka.actor.ActorRef
import scorex.core.network.peer.PeerInfo

/**
  * Peer connected to our node
  *
  * @param connectionId     - connection address
  * @param handlerRef - reference to PeerConnectionHandler that is responsible for communication with this peer
  * @param peerInfo   - information about this peer. May be None if peer is connected, but is not handshaked yet
  */
case class ConnectedPeer(connectionId: ConnectionId,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  override def hashCode(): Int = connectionId.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case p: ConnectedPeer => p.connectionId == this.connectionId && peerInfo == this.peerInfo
    case _ => false
  }

  override def toString: String = s"ConnectedPeer($connectionId)"
}
