package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
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

  type Data = Map[InetSocketAddress, PeerInfo]
  private val DefaultPort = 27017

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  it should "ignore adding self as a peer" in {
    val p = TestProbe()

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val selfAddress = settings.network.bindAddress

    p.send(peerManager, AddOrUpdatePeer(selfAddress, None, None))
    p.send(peerManager, GetAllPeers)
    val data = p.expectMsgClass(classOf[Data])

    data.keySet should not contain selfAddress
  }

  it should "added peer be returned in GetAllPeers" in {
    val p = TestProbe()

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val peerAddress = new InetSocketAddress("1.1.1.1", DefaultPort)

    p.send(peerManager, AddOrUpdatePeer(peerAddress, None, None))
    p.send(peerManager, GetAllPeers)

    val data = p.expectMsgClass(classOf[Data])
    data.keySet should contain(peerAddress)
  }

  it should "remove non public peers" in {
    val p = TestProbe()

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = system.actorOf(Props(new PeerManager(settings, timeProvider)))
    val pa1 = new InetSocketAddress("1.1.1.1", DefaultPort)
    val pa2 = new InetSocketAddress("some_host.com", DefaultPort)

    val h1 = Handshake("test", Version(1: Byte, 2:Byte, 3:Byte), "1", Some(pa1), System.currentTimeMillis())
    //connected peer public cause declared address == peerAddress
    val p1 = ConnectedPeer(pa1, testActor, Incoming, h1)

    p.send(peerManager, Handshaked(p1))
    p.expectNoMessage(1.seconds)
    p.send(peerManager, GetAllPeers)
    val data1 = p.expectMsgClass(classOf[Data])
    data1.keySet should contain(pa1)

    val h2 = Handshake("test", Version(1: Byte, 2:Byte, 3:Byte), "1", Some(pa2), System.currentTimeMillis())
    //connected peer became non-public cause declared address != peerAddress
    val p2 = ConnectedPeer(pa1, testActor, Incoming, h2)

    p.send(peerManager, Handshaked(p2))
    p.expectNoMessage(1.seconds)
    p.send(peerManager, GetAllPeers)
    val data2 = p.expectMsgClass(classOf[Data])
    data2.keySet should not contain pa1
  }
}
