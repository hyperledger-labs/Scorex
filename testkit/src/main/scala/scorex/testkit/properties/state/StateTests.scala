package scorex.testkit.properties.state

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState
import scorex.testkit.CoreGenerators

trait StateTests[
PM <: PersistentNodeViewModifier,
ST <: MinimalState[PM, ST]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
  with PropertyChecks with CoreGenerators {

  val state: ST

}
