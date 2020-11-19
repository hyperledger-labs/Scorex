package spv.serialization

import examples.spv.{Header, HeaderSerializer}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import spv.SPVGenerators

class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
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
