package curvepos

import examples.curvepos.transaction.SimpleBlock
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SimpleBlockTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ExampleGenerators {


  property("Block serialization") {
    forAll(blockGenerator) { b: SimpleBlock =>
      val companion = b.companion
      val recovered = companion.parse(companion.bytes(b)).get
      companion.bytes(b) shouldEqual companion.bytes(recovered)
    }
  }

}
