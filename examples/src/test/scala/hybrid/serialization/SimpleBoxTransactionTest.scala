package hybrid.serialization

import examples.hybrid.state.SimpleBoxTransaction
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SimpleBoxTransactionTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with HybridGenerators {

  property("SimpleBoxTransaction serialization") {
    forAll(simpleBoxTransactionGen) { b: SimpleBoxTransaction =>
      val parsed = b.companion.parse(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }
}
