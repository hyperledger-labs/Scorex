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
      val recovered = companion.parseBytes(companion.toBytes(b)).get
      companion.toBytes(b) shouldEqual companion.toBytes(recovered)

      companion.toBytes(recovered).length shouldEqual SimplePaymentCompanion.TransactionLength
    }
  }

}
