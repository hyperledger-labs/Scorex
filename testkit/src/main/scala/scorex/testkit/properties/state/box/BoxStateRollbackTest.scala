package scorex.testkit.properties.state.box

import org.scalacheck.Gen
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state._
import scorex.core.transaction.BoxTransaction
import scorex.mid.state.BoxMinimalState
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.SemanticallyValidTransactionsCarryingModifier


trait BoxStateRollbackTest[P <: Proposition,
                            TX <: BoxTransaction[P, B],
                            PM <: PersistentNodeViewModifier,
                            CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[P, TX],
                            B <: Box[P],
                            ST <: BoxMinimalState[P, B, TX, PM, ST]]
  extends BoxStateTests[P, B, TX, PM, ST]
    with SemanticallyValidTransactionsCarryingModifier[P, TX, PM, CTM, ST]
    with TestkitHelpers {

  def stateChangesGenerator(state: ST): Gen[BoxStateChanges[P, B]]
  val transactionGenerator: Gen[TX]

  property("State version updates as expected") {

    forAll(stateGen, minSuccessful(2)) {state =>
      var lastVersion = state.version
      var newState = state

        forAll(modifierIdGen) {newVersion =>
          lastVersion shouldEqual newState.version

          val c = stateChangesGenerator(newState).sample.get

          newState = newState.applyChanges(c, newVersion).get
          newState.version shouldBe newVersion
          lastVersion = newVersion
        }
    }
  }

  property("State changes application and rollback leads to the same state") {

    forAll(stateGen, minSuccessful(2)) { state =>

      var newState = state
      var rollbackVersionOpt: Option[Array[Byte]] = None

      forAll(modifierIdGen) { newVersion =>
        val c = stateChangesGenerator(newState).sample.get

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

  property("State changes application and rollback leads to rollback of changes") {

    forAll(stateGen, minSuccessful(2)) { state =>

      var newState = state
      var rollbackVersionOpt: Option[Array[Byte]] = None
      var txPair = genValidTransactionPair(state)

      check { _ =>
        rollbackVersionOpt match {
          case None =>
            val randomTx = txPair.head
            val block = semanticallyValidModifierWithCustomTransactions(state, Seq(randomTx))
            val blockChanges = newState.changes(block).get

            val changes: BoxStateChanges[P, B] = BoxStateChanges(blockChanges.operations.flatMap { op =>
              op match {
                case rm: Removal[P, B] if newState.closedBox(rm.boxId).isEmpty => None
                case _ => Some(op)
              }
            })
            newState = newState.applyChanges(changes, block.id).get
            rollbackVersionOpt = Some(newState.version)

          case Some(rollbackVersion) =>
            val randomTx = txPair(1)
            val block = semanticallyValidModifierWithCustomTransactions(state, Seq(randomTx))
            val blockChanges = newState.changes(block).get

            val changes: BoxStateChanges[P, B] = BoxStateChanges(blockChanges.operations.flatMap { op =>
              op match {
                case rm: Removal[P, B] if newState.closedBox(rm.boxId).isEmpty => None
                case _ => Some(op)
              }
            })
            newState = newState.applyChanges(changes, block.id).get

            changes.toAppend.foreach { b =>
              newState.closedBox(b.box.id).isDefined shouldBe true
            }
            changes.toRemove.foreach { r =>
              newState.closedBox(r.boxId).isDefined shouldBe false
            }

            newState = newState.rollbackTo(rollbackVersion).get

            changes.toAppend.foreach { b =>
              newState.closedBox(b.box.id).isDefined shouldBe false
            }
            changes.toRemove.foreach { r =>
              newState.closedBox(r.boxId).isDefined shouldBe true
            }
            // Since transactions are created in pair where the first transaction creates a box and the second transaction uses
            // this box, so once the pair has been utilized we need to use a new pair for the further iterations.
            rollbackVersionOpt = None
            txPair = genValidTransactionPair(state)
        }
      }
    }
  }
}