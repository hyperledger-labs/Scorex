package hybrid.primitives

import examples.commons.PublicKey25519NoncedBox
import hybrid.HybridGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.state.PrivateKey25519Companion

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class PrivateKey25519Suite extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators {

  property("Public key is deterministic") {
    forAll(genBytes(32), minSuccessful(1000)){ seed =>
      val pair1 = PrivateKey25519Companion.generateKeys(seed)
      val pair2 = PrivateKey25519Companion.generateKeys(seed)

      pair1._1.privKeyBytes shouldBe pair2._1.privKeyBytes
      pair1._1.publicKeyBytes shouldBe pair2._1.publicKeyBytes
      pair1._1.publicKeyBytes shouldBe pair2._2.pubKeyBytes
      pair1._1.publicKeyBytes shouldBe pair2._2.bytes
    }
  }

  property("PublicKey25519NoncedBox right owner") {
    forAll(key25519Gen, nonceGen, valueGen, minSuccessful(1000)){ case(keyPair, nonce, amount) =>
      val box = PublicKey25519NoncedBox(keyPair._2, nonce, amount)
        PrivateKey25519Companion.owns(keyPair._1, box) shouldBe true
    }
  }
}
