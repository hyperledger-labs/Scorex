package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest._
import scorex.ObjectGenerators
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

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
}
