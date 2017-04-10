package tailchain.serialization

import examples.tailchain.core.PartialProof
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import tailchain.TailchainGenerators

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TailchainGenerators {

  property("PartialProof serialization") {
    forAll(partialProofGen) { b: PartialProof =>
      val parsed = PartialProof.parseBytes(PartialProof.toBytes(b)).get
      PartialProof.toBytes(b) shouldEqual PartialProof.toBytes(parsed)
    }
  }

}
