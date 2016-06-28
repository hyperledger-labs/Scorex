package scorex.account

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AccountSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  ignore("25519 - Account.fromPublicKey should generate valid account - ???") {
    forAll { data: Array[Byte] =>

    }
  }

}
