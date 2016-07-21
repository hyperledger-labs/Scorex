package scorex.crypto

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.state.SecretGenerator25519


class SigningFunctionsSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("SecretGenerator25519 generate valid keypair") {
    forAll() { (seed1: Array[Byte], message1: Array[Byte], seed2: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val priv = SecretGenerator25519.generateKeys(seed1)
        val priv2 = SecretGenerator25519.generateKeys(seed2)
        val sig = priv.sign(message1)
        sig.isValid(priv.publicCommitment, message1) shouldBe true

        sig.isValid(priv.publicCommitment, message2) shouldBe false
        sig.isValid(priv2.publicCommitment, message1) shouldBe false
      }
    }
  }
}