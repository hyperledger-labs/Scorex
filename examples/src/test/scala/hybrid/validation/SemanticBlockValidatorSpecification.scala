package hybrid.validation

import examples.hybrid.validation.SemanticBlockValidator
import hybrid.HybridGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.hash.Blake2b256


class SemanticBlockValidatorSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators {

  private val validator = new SemanticBlockValidator(Blake2b256)

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
