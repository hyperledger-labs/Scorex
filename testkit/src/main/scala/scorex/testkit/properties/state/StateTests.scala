package scorex.testkit.properties.state

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.{SemanticallyValidModifierProducer, SemanticallyInvalidModifierProducer, CoreGenerators}

trait StateTests[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]]
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreGenerators
    with TestkitHelpers
    with SemanticallyValidModifierProducer[PM, ST]
    with SemanticallyInvalidModifierProducer[PM, ST] {

  val checksToMake = 10

  val stateGen: Gen[ST]
}
