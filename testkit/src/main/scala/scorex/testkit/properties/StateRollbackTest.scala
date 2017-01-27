package scorex.testkit.properties

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{MinimalState, StateChanges}

trait StateRollbackTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST]] extends StateTests[P, TX, PM, B, ST] {

  val stateChangesGenerator: Gen[StateChanges[P, B]]

  property("State changes application and rollback leads to the same state") {
    forAll(stateChangesGenerator, modifierIdGen) { (c, newVersion) =>
      val oldVersion = state.version

      val newState: ST = state.applyChanges(c, newVersion).get
      newState.version shouldBe newVersion

      newState.rollbackTo(oldVersion).get.version shouldEqual oldVersion
    }
  }


}
