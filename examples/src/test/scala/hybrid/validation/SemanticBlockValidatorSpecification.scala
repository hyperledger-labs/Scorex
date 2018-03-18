package hybrid.validation

import examples.hybrid.validation.SemanticBlockValidator
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256


class SemanticBlockValidatorSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val validator: SemanticBlockValidator = new SemanticBlockValidator(Blake2b256)

  property("Generated PoS block semantics is valid") {
    forAll(posBlockGen) { posBlock =>
      validator.validate(posBlock).get
    }
  }

  property("Generated PoW block semantics is valid") {
    forAll(powBlockGen) { powBlock =>
      validator.validate(powBlock).get
    }
  }
}
