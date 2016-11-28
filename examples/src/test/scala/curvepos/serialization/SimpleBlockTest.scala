package curvepos.serialization

import curvepos.ExampleGenerators
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
      val companion = b.serializer
      val recovered = companion.parseBytes(companion.toBytes(b)).get
      companion.toBytes(b) shouldEqual companion.toBytes(recovered)
    }
  }

}
