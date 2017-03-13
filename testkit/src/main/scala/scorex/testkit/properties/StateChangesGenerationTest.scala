package scorex.testkit.properties

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{MinimalState, StateChanges}

trait StateChangesGenerationTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends StateTests[P, TX, PM, B, ST] {

  val history: HT
  def genValidModifier(history: HT): PM

  property("State should be able to generate changes from block and apply them") {
    (0 until 100) foreach { _ =>
      val block = genValidModifier(history)
      val blockChanges = state.changes(block).get
      val existingBoxIds = blockChanges.boxIdsToRemove.filter(bi => state.closedBox(bi).isDefined)
      val changes: StateChanges[P, B] = blockChanges.copy(boxIdsToRemove = existingBoxIds)
      val newState = state.applyChanges(changes, block.id).get
      changes.toAppend.foreach { b =>
        newState.closedBox(b.id).isDefined shouldBe true
      }
      changes.boxIdsToRemove.foreach { bId =>
        newState.closedBox(bId).isDefined shouldBe false
      }
    }
  }


}
