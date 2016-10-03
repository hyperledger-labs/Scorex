package curvepos

import examples.curvepos.transaction.{SimpleBlockCompanion, SimplePayment}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SimplePaymentTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ExampleGenerators {


  property("Payment serialization") {
    forAll(paymentGen) { b: SimplePayment =>
      val companion = b.companion
      val recovered = companion.parse(companion.bytes(b)).get
      companion.bytes(b) shouldEqual companion.bytes(recovered)
    }
  }

}
