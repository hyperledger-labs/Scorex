package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.network.NetworkTests

class InMemoryPeerDatabaseSpec extends NetworkTests {

  private val peerAddress1 = new InetSocketAddress("1.1.1.1", 27017)
  private val peerAddress2 = new InetSocketAddress("2.2.2.2", 27017)

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

    db.addOrUpdateKnownPeer(getPeerInfo(peerAddress1))
    db.isEmpty shouldBe false
    db.blacklistedPeers.isEmpty shouldBe true
    db.knownPeers.isEmpty shouldBe false
  }

  it should "return a peer after adding a peer" in {
    val db = newDb
    val peerInfo = getPeerInfo(peerAddress1)

    db.addOrUpdateKnownPeer(peerInfo)
    db.knownPeers shouldBe Map(peerAddress1 -> peerInfo)
  }

  it should "return an updated peer after updating a peer" in {
    val db = newDb
    val peerInfo = getPeerInfo(peerAddress1, Some("initialName"))
    db.addOrUpdateKnownPeer(peerInfo)
    val newPeerInfo = getPeerInfo(peerAddress1, Some("updatedName"))
    db.addOrUpdateKnownPeer(newPeerInfo)

    db.knownPeers shouldBe Map(peerAddress1 -> newPeerInfo)
  }

  it should "return a blacklisted peer after blacklisting" in {
    val db = newDb
    db.addOrUpdateKnownPeer(getPeerInfo(peerAddress1))
    db.addOrUpdateKnownPeer(getPeerInfo(peerAddress2))
    db.addToBlacklist(peerAddress1)

    db.isBlacklisted(peerAddress1.getAddress) shouldBe true
    db.isBlacklisted(peerAddress2.getAddress) shouldBe false
    db.blacklistedPeers shouldBe Seq(peerAddress1.getAddress)
  }

  it should "the blacklisted peer be absent in knownPeers" in {
    val db = newDb
    val peerInfo1 = getPeerInfo(peerAddress1)
    db.addOrUpdateKnownPeer(peerInfo1)
    db.addOrUpdateKnownPeer(getPeerInfo(peerAddress2))
    db.addToBlacklist(peerAddress2)

    db.knownPeers shouldBe Map(peerAddress1 -> peerInfo1)
  }

  it should "remove peers from db correctly" in {
    val db = newDb

    db.addOrUpdateKnownPeer(getPeerInfo(peerAddress1))
    db.isEmpty shouldBe false
    db.blacklistedPeers.isEmpty shouldBe true
    db.knownPeers.isEmpty shouldBe false

    db.remove(peerAddress1)

    db.isEmpty shouldBe true
  }

}
