package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest._
import scorex.ObjectGenerators
import scorex.core.app.{ScorexContext, Version}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

class PeerManagerSpec extends FlatSpec with Matchers with ObjectGenerators {

  import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, GetAllPeers}

  type Data = Map[InetSocketAddress, PeerInfo]
  private val DefaultPort = 27017
  private val settings = ScorexSettings.read(None)
  private val timeProvider = new NetworkTimeProvider(settings.ntp)

  it should "ignore adding self as a peer" in {
    implicit val system = ActorSystem()
    val p = TestProbe("p")(system)
    implicit val defaultSender: ActorRef = p.testActor


    val selfAddress = settings.network.bindAddress
    val scorexContext = ScorexContext(Seq.empty, Seq.empty, None, timeProvider, Some(selfAddress))
    val peerManager = PeerManagerRef(settings, scorexContext)(system)
    val peerInfo = PeerInfo(timeProvider.time(), Some(selfAddress), Version.last)

    peerManager ! AddOrUpdatePeer(peerInfo.handshake)
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
    val scorexContext = ScorexContext(Seq.empty, Seq.empty, None, timeProvider, None)
    val peerManager = PeerManagerRef(settings, scorexContext)(system)
    val peerAddress = new InetSocketAddress("1.1.1.1", DefaultPort)
    val peerInfo = PeerInfo(timeProvider.time(), Some(peerAddress), Version.last)

    peerManager ! AddOrUpdatePeer(peerInfo.handshake)
    peerManager ! GetAllPeers

    val data = p.expectMsgClass(classOf[Data])
    data.keySet should contain(peerAddress)
    system.terminate()
  }
}
