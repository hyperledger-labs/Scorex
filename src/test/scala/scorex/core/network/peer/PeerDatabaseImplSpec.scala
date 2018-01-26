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
}
