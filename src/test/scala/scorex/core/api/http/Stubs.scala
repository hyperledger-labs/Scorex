package scorex.core.api.http

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorSystem, Props}
import scorex.core.app.Version
import scorex.core.network.Handshake
import scorex.core.network.peer.{PeerInfo, PeerManager}

trait Stubs {

  implicit val system: ActorSystem

  val inetAddr1 = new InetSocketAddress("92.92.92.92",27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93",27017)
  val ts1 = System.currentTimeMillis() - 100
  val ts2 = System.currentTimeMillis() + 100

  val peers = Map(
    inetAddr1 -> PeerInfo(ts1, Some(1L), Some("first")),
    inetAddr2 -> PeerInfo(ts2, Some(2L), Some("second"))
  )

  val protocolVersion = Version("1.1.1")

  val connectedPeers = Seq(
    Handshake("node_pop", protocolVersion, "first", Some(inetAddr1), ts1),
    Handshake("node_pop", protocolVersion, "second", Some(inetAddr2), ts2)
  )

  val blacklistedPeers = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  class PeersManagerStub extends Actor {
    def receive = {
      case PeerManager.GetConnectedPeers => sender() ! connectedPeers
      case PeerManager.GetAllPeers => sender() ! peers
      case PeerManager.GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props() = Props(new PeersManagerStub)
  }

  class NetworkControllerStub extends Actor {
    def receive = { case _ => () }
  }

  object NetworkControllerStub {
    def props() = Props(new NetworkControllerStub)
  }

  lazy val pmRef = system.actorOf(PeersManagerStub.props())
  lazy val networkControllerRef = system.actorOf(NetworkControllerStub.props())

}
