package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest._
import scorex.core.app.Version
import scorex.core.network.peer.PeerManager.ReceivableMessages.Handshaked
import scorex.core.network.{ConnectedPeer, Handshake, Incoming}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._

class PeerManagerSpec extends TestKit(ActorSystem("PeerManager"))
  with ImplicitSender
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll {

  import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, GetAllPeers}

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  it should "ignore adding self as a peer" in {
    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val selfAddress = settings.network.bindAddress
    peerManager ! AddOrUpdatePeer(selfAddress, None, None)
    peerManager ! GetAllPeers
    receiveOne(1.second).asInstanceOf[Map[InetSocketAddress, PeerInfo]].contains(selfAddress) shouldBe false
  }

  it should "added peer be returned in GetAllPeers" in {
    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val peerAddress = new InetSocketAddress("1.1.1.1",27017)
    peerManager ! AddOrUpdatePeer(peerAddress, None, None)
    peerManager ! GetAllPeers
    receiveOne(1.second).asInstanceOf[Map[InetSocketAddress, PeerInfo]].contains(peerAddress) shouldBe true
  }

  it should "remove non public peers" in {
    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val pa1 = new InetSocketAddress("1.1.1.1",27017)
    val pa2 = new InetSocketAddress("some_host.com", 27017)

    val h1 = Handshake("test", Version(1: Byte, 2:Byte, 3:Byte), "1", Some(pa1), System.currentTimeMillis())
    //connected peer public cause declared address == peerAddress
    val p1 = ConnectedPeer(pa1, testActor, Incoming, h1)

    peerManager ! Handshaked(p1)
    expectNoMessage(1.second)
    peerManager ! GetAllPeers
    receiveOne(1.second).asInstanceOf[Map[InetSocketAddress, PeerInfo]].contains(pa1) shouldBe true

    val h2 = Handshake("test", Version(1: Byte, 2:Byte, 3:Byte), "1", Some(pa2), System.currentTimeMillis())
    //connected peer became non-public cause declared address != peerAddress
    val p2 = ConnectedPeer(pa1, testActor, Incoming, h2)

    peerManager ! Handshaked(p2)
    expectNoMessage(1.second)
    peerManager ! GetAllPeers
    receiveOne(1.second).asInstanceOf[Map[InetSocketAddress, PeerInfo]].contains(pa1) shouldBe false
  }
}
