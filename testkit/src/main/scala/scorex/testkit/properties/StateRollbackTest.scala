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

  property("State version updates as expected") {

    var lastVersion = state.version
    var newState = state

    forAll(stateChangesGenerator, modifierIdGen) { (c, newVersion) =>
      lastVersion shouldEqual newState.version

      newState = newState.applyChanges(c, newVersion).get
      newState.version shouldBe newVersion
      lastVersion = newVersion
    }
  }


  property("State changes application and rollback leads to the same state") {

    var newState = state
    var rollbackVersionOpt: Option[Array[Byte]] = None

    forAll(stateChangesGenerator, modifierIdGen) { (c, newVersion) =>
      rollbackVersionOpt match {
        case None =>
          newState = newState.applyChanges(c, newVersion).get
          rollbackVersionOpt = Some(newState.version)
        case Some(rollbackVersion) =>

          rollbackVersion shouldEqual newState.version

          newState = newState.applyChanges(c, newVersion).get
          newState.version shouldBe newVersion

          newState = newState.rollbackTo(rollbackVersion).get
          newState.version shouldEqual rollbackVersion
      }
    }
  }


}
