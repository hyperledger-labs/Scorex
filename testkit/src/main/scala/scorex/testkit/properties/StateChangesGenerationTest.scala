package scorex.testkit.properties

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.StateChanges

trait StateChangesGenerationTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P]] extends StateTests[P, TX, PM, B] {

  val validBlockGenerator: Gen[PM]

  property("State should be able to generate changes from block and apply them") {
    forAll(validBlockGenerator) { block =>
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
