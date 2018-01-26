package scorex.core.network.peer

import java.net.InetSocketAddress

import org.scalatest.{FlatSpec, Matchers}

class PeerDatabaseImplSpec extends FlatSpec
  with Matchers {

  private val bindAddr = new InetSocketAddress("92.92.92.92",27017)

  "new DB" should "be empty" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    db.isEmpty() shouldBe true
    db.blacklistedPeers().isEmpty shouldBe true
    db.knownPeers(false).isEmpty shouldBe true
    db.knownPeers(true).isEmpty shouldBe true
  }

  "new DB" should "be non-empty after adding a peer" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    val peerAddress = new InetSocketAddress("1.1.1.1",27017)
    val lastSeen = System.currentTimeMillis()
    db.addOrUpdateKnownPeer(peerAddress, PeerInfo(lastSeen))
    db.isEmpty() shouldBe false
    db.blacklistedPeers().isEmpty shouldBe true
    db.knownPeers(false).isEmpty shouldBe false
    db.knownPeers(true).isEmpty shouldBe false
  }

  "new DB" should "return a peer after adding a peer" in {
    val db = new PeerDatabaseImpl(bindAddr, None, None)
    val peerAddress = new InetSocketAddress("1.1.1.1",27017)
    val lastSeen = System.currentTimeMillis()
    val peerInfo = PeerInfo(lastSeen)
    db.addOrUpdateKnownPeer(peerAddress,  peerInfo)
    db.knownPeers(true) shouldBe Map(peerAddress -> peerInfo)
  }

}
