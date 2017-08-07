package scorex.testkit.properties

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.{BoxTransaction, Transaction}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{BoxStateChanges, Removal}
import scorex.mid.state.BoxMinimalState
import scorex.testkit.TestkitHelpers

trait BoxStateChangesGenerationTest[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: BoxMinimalState[P, B, TX, PM, ST],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends StateTests[P, TX, PM, B, ST] with TestkitHelpers {

  val history: HT

  def genValidModifier(history: HT, mempoolTransactionFetchOption: Boolean, noOfTransactionsFromMempool : Int): PM

  property("State should be able to generate changes from block and apply them") {
    check { _ =>
      val block = genValidModifier(history, false, 0)
      val blockChanges = state.changes(block).get

      val changes: BoxStateChanges[P, B] = BoxStateChanges(blockChanges.operations.flatMap{ op =>
        op match {
          case rm: Removal[P, B] if state.closedBox(rm.boxId).isEmpty => None
          case _ => Some(op)
        }
      })

      val newState = state.applyChanges(changes, block.id).get
      changes.toAppend.foreach { b =>
        newState.closedBox(b.box.id).isDefined shouldBe true
      }
      changes.toRemove.foreach { r =>
        newState.closedBox(r.boxId).isDefined shouldBe false
      }
    }
  }
}
