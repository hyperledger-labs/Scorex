package scorex.core.api.http

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.app.Version
import scorex.core.network.peer.PeerInfo
import scorex.core.network._

trait Stubs {

  implicit val system: ActorSystem

  private val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  private val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)
  private val ts1 = System.currentTimeMillis() - 100
  private val ts2 = System.currentTimeMillis() + 100
  private val version: Version = Version.initial

  val peerFeatures: Seq[PeerFeature] = Seq()

  val peers = Map(
    inetAddr1 -> PeerInfo(PeerSpec("app", version, "first", Some(inetAddr1), peerFeatures), ts1, Some(Incoming)),
    inetAddr2 -> PeerInfo(PeerSpec("app", version, "second", Some(inetAddr2), peerFeatures), ts1, Some(Outgoing))
  )

  val protocolVersion = Version("1.1.1")

  val connectedPeers = Seq(
    Handshake(PeerSpec("node_pop", protocolVersion, "first", Some(inetAddr1), peerFeatures), ts1),
    Handshake(PeerSpec("node_pop", protocolVersion, "second", Some(inetAddr2), peerFeatures), ts2)
  )

  val blacklistedPeers = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  class PeersManagerStub extends Actor {

    import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}

    def receive = {
      //      case GetConnectedPeers => sender() ! connectedPeers
      case GetAllPeers => sender() ! peers
      case GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStubRef {
    def props(): Props = Props(new PeersManagerStub)

    def apply()(implicit system: ActorSystem): ActorRef = system.actorOf(props())

    def apply(name: String)(implicit system: ActorSystem): ActorRef = system.actorOf(props(), name)
  }

  class NetworkControllerStub extends Actor {
    def receive = {
      case _ => ()
    }
  }

  object NetworkControllerStubRef {
    def props(): Props = Props(new NetworkControllerStub)

    def apply()(implicit system: ActorSystem): ActorRef = system.actorOf(props())

    def apply(name: String)(implicit system: ActorSystem): ActorRef = system.actorOf(props(), name)
  }

  lazy val pmRef: ActorRef = PeersManagerStubRef()
  lazy val networkControllerRef: ActorRef = NetworkControllerStubRef()

}
