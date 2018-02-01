package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest._
import scorex.core.network.peer.PeerManager.{AddOrUpdatePeer, GetAllPeers}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scala.concurrent.duration._

class PeerManagerSpec extends TestKit(ActorSystem("PeerManager"))
  with ImplicitSender
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll {

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
}
