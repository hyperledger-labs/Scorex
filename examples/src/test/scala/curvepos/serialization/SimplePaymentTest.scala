package curvepos.serialization

import curvepos.ExampleGenerators
import examples.curvepos.transaction.{SimplePayment, SimplePaymentCompanion, SimpleTransaction}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SimplePaymentTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ExampleGenerators {


  property("Payment serialization") {
    forAll(paymentGen) { b: SimplePayment =>
      val companion = b.serializer
      val recovered = companion.parseBytes(companion.bytes(b)).get
      companion.bytes(b) shouldEqual companion.bytes(recovered)

      companion.bytes(recovered).length shouldEqual SimplePaymentCompanion.TransactionLength
    }
  }

}
