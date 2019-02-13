package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.ActorRef

case class ConnectedPeer(remoteAddress: InetSocketAddress,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  import shapeless.syntax.typeable._

  override def hashCode(): Int = remoteAddress.hashCode()

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(p => p.remoteAddress == this.remoteAddress && peerInfo == this.peerInfo)

  override def toString: String = s"ConnectedPeer($remoteAddress)"
}
