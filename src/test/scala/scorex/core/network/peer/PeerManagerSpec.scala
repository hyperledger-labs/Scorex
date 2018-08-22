package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest._
import scorex.ObjectGenerators
import scorex.core.app.Version
import scorex.core.network.peer.PeerManager.ReceivableMessages.Handshaked
import scorex.core.network.{ConnectedPeer, Handshake, Incoming}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class PeerManagerSpec extends FlatSpec with Matchers with ObjectGenerators {

  import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, GetAllPeers}

  type Data = Map[InetSocketAddress, PeerInfo]
  private val DefaultPort = 27017

  it should "ignore adding self as a peer" in {
    implicit val system = ActorSystem()
    val p = TestProbe("p")(system)
    implicit val defaultSender: ActorRef = p.testActor

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)

    val selfAddress = settings.network.bindAddress
    val peerManager = PeerManagerRef(settings, timeProvider, Some(selfAddress))(system)

    peerManager ! AddOrUpdatePeer(selfAddress, None, None, Seq())
    peerManager ! GetAllPeers
    val data = p.expectMsgClass(classOf[Data])

    data.keySet should not contain selfAddress
    system.terminate()
  }

  it should "added peer be returned in GetAllPeers" in {
    implicit val system = ActorSystem()
    val p = TestProbe("p")(system)
    implicit val defaultSender: ActorRef = p.testActor

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = PeerManagerRef(settings, timeProvider, None)(system)
    val peerAddress = new InetSocketAddress("1.1.1.1", DefaultPort)

    peerManager ! AddOrUpdatePeer(peerAddress, None, None, Seq())
    peerManager ! GetAllPeers

    val data = p.expectMsgClass(classOf[Data])
    data.keySet should contain(peerAddress)
    system.terminate()
  }

  it should "remove non public peers" in {
    implicit val system = ActorSystem()
    val pr1 = TestProbe("p1")(system)
    val pr2 = TestProbe("p2")(system)
    val testActor = pr2.testActor
    implicit val defaultSender: ActorRef = pr1.testActor

    val settings = ScorexSettings.read(None)
    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = PeerManagerRef(settings, timeProvider, None)(system)
    val pa1 = new InetSocketAddress("1.1.1.1", DefaultPort)
    val pa2 = new InetSocketAddress("some_host.com", DefaultPort)

    val feats = Seq(FullNodePeerFeature)

    val h1 = Handshake("test", Version(1: Byte, 2: Byte, 3: Byte), "1", Some(pa1), feats, System.currentTimeMillis())
    //connected peer is public cause declared address is defined
    val p1 = ConnectedPeer(pa1, testActor, Incoming, h1)

    peerManager ! Handshaked(p1)
    pr1.expectNoMessage(1.seconds)
    peerManager ! GetAllPeers
    val data1 = pr1.expectMsgClass(classOf[Data])
    data1.keySet should contain(pa1)

    val h2 = Handshake("test", Version(1: Byte, 2: Byte, 3: Byte), "1", None, feats, System.currentTimeMillis())
    //connected peer became non-public cause declared address is not defined
    val p2 = ConnectedPeer(pa1, testActor, Incoming, h2)

    peerManager ! Handshaked(p2)
    pr1.expectNoMessage(1.seconds)
    peerManager ! GetAllPeers
    val data2 = pr1.expectMsgClass(classOf[Data])
    data2.keySet should not contain pa1
    system.terminate()
  }
}
