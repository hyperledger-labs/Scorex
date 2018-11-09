package spv.serialization

import examples.spv.{Header, HeaderSerializer}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import spv.SPVGenerators

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with SPVGenerators {

  property("BlockHeader serialization") {
    forAll(blockHeaderGen) { b: Header =>
      val serializer = HeaderSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }
}
