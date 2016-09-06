package scorex.crypto

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion



class SigningFunctionsSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("PrivateKey25519Companion generates valid keypair") {
    forAll() { (seed1: Array[Byte], message1: Array[Byte], seed2: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val priv = PrivateKey25519Companion.generateKeys(seed1)._1
        val priv2 = PrivateKey25519Companion.generateKeys(seed2)._1
        val sig = PrivateKey25519Companion.sign(priv, message1)
        sig.isValid(PublicKey25519Proposition(priv.publicImage), message1) shouldBe true

        sig.isValid(PublicKey25519Proposition(priv.publicImage), message2) shouldBe false
        sig.isValid(PublicKey25519Proposition(priv2.publicImage), message1) shouldBe false
      }
    }
  }

  property("PrivateKey25519Companion serialization") {
    forAll() { (seed: Array[Byte], message: Array[Byte]) =>
      val priv = PrivateKey25519Companion.generateKeys(seed)._1
      val parsed = PrivateKey25519Companion.parseBytes(PrivateKey25519Companion.keyPairBytes(priv))

      parsed.isSuccess shouldBe true
      parsed.get._2.address shouldBe priv.publicImage.address
      PrivateKey25519Companion.sign(parsed.get._1, message)
        .isValid(PublicKey25519Proposition(priv.publicImage), message) shouldBe true
    }
  }

}