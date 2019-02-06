package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.ActorRef

case class ConnectedPeer(remote: InetSocketAddress,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  import shapeless.syntax.typeable._

  override def hashCode(): Int = remote.hashCode()

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(p => p.remote == this.remote && peerInfo == this.peerInfo)

  override def toString: String = s"ConnectedPeer($remote)"
}
