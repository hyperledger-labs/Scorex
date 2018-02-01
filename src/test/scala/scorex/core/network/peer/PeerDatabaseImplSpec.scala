package scorex.core.network.peer

import java.net.InetSocketAddress

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.utils.NetworkTime

class PeerDatabaseImplSpec extends FlatSpec
  with Matchers {

  private val bindAddr = new InetSocketAddress("92.92.92.92",27017)

  private val peerAddress1 = new InetSocketAddress("1.1.1.1",27017)
  private val peerAddress2 = new InetSocketAddress("2.2.2.2",27017)

  private def currentTime(): NetworkTime.Time = System.currentTimeMillis()

  "new DB" should "be empty" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)

    db.isEmpty() shouldBe true
    db.blacklistedPeers().isEmpty shouldBe true
    db.knownPeers().isEmpty shouldBe true
    db.knownPeers().isEmpty shouldBe true
  }

   it should "be non-empty after adding a peer" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    db.addOrUpdateKnownPeer(peerAddress1, PeerInfo(currentTime()))

    db.isEmpty() shouldBe false
    db.blacklistedPeers().isEmpty shouldBe true
    db.knownPeers().isEmpty shouldBe false
    db.knownPeers().isEmpty shouldBe false
  }

  it should "return a peer after adding a peer" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    val peerInfo = PeerInfo(currentTime())
    db.addOrUpdateKnownPeer(peerAddress1,  peerInfo)

    db.knownPeers() shouldBe Map(peerAddress1 -> peerInfo)
  }

  it should "return an updated peer after updating a peer" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    val peerInfo = PeerInfo(currentTime())
    db.addOrUpdateKnownPeer(peerAddress1,  peerInfo)
    val newPeerInfo = PeerInfo(currentTime())
    db.addOrUpdateKnownPeer(peerAddress1, newPeerInfo)

    db.knownPeers() shouldBe Map(peerAddress1 -> newPeerInfo)
  }

  it should "return a blacklisted peer after blacklisting" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    db.addOrUpdateKnownPeer(peerAddress1, PeerInfo(currentTime()))
    db.addOrUpdateKnownPeer(peerAddress2, PeerInfo(currentTime()))
    db.blacklistPeer(peerAddress1, currentTime())

    db.isBlacklisted(peerAddress1) shouldBe true
    db.isBlacklisted(peerAddress2) shouldBe false
    db.blacklistedPeers() shouldBe Seq(peerAddress1.getHostName)
  }
}
