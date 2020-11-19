package scorex.core.transaction.box.proposition

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.state.PrivateKey25519Companion

class PublicKey25519PropositionSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  property("PublicKey25519Proposition generates valid addresses") {
    forAll() { (seed: Array[Byte]) =>
      val pub = PrivateKey25519Companion.generateKeys(seed)._2
      PublicKey25519Proposition.validPubKey(pub.address).isSuccess shouldBe true
    }
  }
}