package scorex.core.network.peer

import java.net.InetSocketAddress

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.utils.TimeProvider.Time

import scala.concurrent.ExecutionContext.Implicits._

class InMemoryPeerDatabaseSpec extends FlatSpec with Matchers {

  private val peerAddress1 = new InetSocketAddress("1.1.1.1", 27017)
  private val peerAddress2 = new InetSocketAddress("2.2.2.2", 27017)

  val settings: ScorexSettings = ScorexSettings.read(None)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  def currentTime: Time = timeProvider.time()

  def newDb: InMemoryPeerDatabase = new InMemoryPeerDatabase(settings, timeProvider)

  "new DB" should "be empty" in {
    val db = newDb

    db.isEmpty shouldBe true
    db.blacklistedPeers.isEmpty shouldBe true
    db.knownPeers.isEmpty shouldBe true
    db.knownPeers.isEmpty shouldBe true
  }

   it should "be non-empty after adding a peer" in {
     val db = newDb

     db.addOrUpdateKnownPeer(PeerInfo(currentTime, Some(peerAddress1)))
     db.isEmpty shouldBe false
     db.blacklistedPeers.isEmpty shouldBe true
     db.knownPeers.isEmpty shouldBe false
  }

  it should "return a peer after adding a peer" in {
    val db = newDb
    val peerInfo = PeerInfo(currentTime, declaredAddress = Some(peerAddress1))

    db.addOrUpdateKnownPeer(peerInfo)
    db.knownPeers shouldBe Map(peerAddress1 -> peerInfo)
  }

  it should "return an updated peer after updating a peer" in {
    val db = newDb
    val peerInfo = PeerInfo(currentTime, Some(peerAddress1))
    db.addOrUpdateKnownPeer(peerInfo)
    val newPeerInfo = PeerInfo(currentTime,Some(peerAddress1))
    db.addOrUpdateKnownPeer(newPeerInfo)

    db.knownPeers shouldBe Map(peerAddress1 -> newPeerInfo)
  }

  it should "return a blacklisted peer after blacklisting" in {
    val db = newDb
    db.addOrUpdateKnownPeer(PeerInfo(currentTime, Some(peerAddress1)))
    db.addOrUpdateKnownPeer(PeerInfo(currentTime, Some(peerAddress2)))
    db.addToBlacklist(peerAddress1)

    db.isBlacklisted(peerAddress1) shouldBe true
    db.isBlacklisted(peerAddress2) shouldBe false
    db.blacklistedPeers shouldBe Seq(peerAddress1.getHostName)
  }

  it should "the blacklisted peer be absent in knownPeers" in {
    val db = newDb
    val peerInfo1 = PeerInfo(currentTime, Some(peerAddress1))
    db.addOrUpdateKnownPeer(peerInfo1)
    db.addOrUpdateKnownPeer(PeerInfo(currentTime, Some(peerAddress2)))
    db.addToBlacklist(peerAddress2)

    db.knownPeers shouldBe Map(peerAddress1 -> peerInfo1)
  }

  it should "remove peers from db correctly" in {
    val db = newDb

    db.remove(peerAddress1) shouldBe false
    db.addOrUpdateKnownPeer(PeerInfo(currentTime, Some(peerAddress1)))
    db.isEmpty shouldBe false
    db.blacklistedPeers.isEmpty shouldBe true
    db.knownPeers.isEmpty shouldBe false

    db.remove(peerAddress1) shouldBe true
    db.isEmpty shouldBe true
  }

}
