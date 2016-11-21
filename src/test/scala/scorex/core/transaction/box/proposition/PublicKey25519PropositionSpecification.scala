package scorex.core.transaction.box.proposition

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.PrivateKey25519Companion

class PublicKey25519PropositionSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("PublicKey25519Proposition generates valid addresses") {
    forAll() { (seed: Array[Byte]) =>
      val pub = PrivateKey25519Companion.generateKeys(seed)._2
      PublicKey25519Proposition.validPubKey(pub.address).isSuccess shouldBe true
    }
  }
}