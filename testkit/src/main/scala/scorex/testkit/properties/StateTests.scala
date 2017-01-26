package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.testkit.CoreGenerators

trait StateTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks with CoreGenerators {
  type ST = MinimalState[P, B, TX, PM, _ <: MinimalState[P, B, TX, PM, _]]
  val state: ST

}
