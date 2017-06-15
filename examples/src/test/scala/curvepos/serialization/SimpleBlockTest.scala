package curvepos.serialization

import curvepos.ExampleGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.SerializationTests

class SimpleBlockTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ExampleGenerators
  with SerializationTests {


  property("Block serialization") {
    checkSerializationRoundtrip(blockGenerator)
  }

}
