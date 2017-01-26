package scorex.testkit.properties

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState

trait StateTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks{
  type ST = MinimalState[P, B, TX, PM, _ <: MinimalState[P, B, TX, PM, _]]
  val state: ST

}
