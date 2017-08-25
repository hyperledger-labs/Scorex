package scorex.testkit.properties.state

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.{CoreGenerators, SemanticallyValidModifierProducer}

trait StateTests[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]]
  extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with PropertyChecks
    with CoreGenerators
    with TestkitHelpers
    with SemanticallyValidModifierProducer[PM, ST] {

  val state: ST

}
