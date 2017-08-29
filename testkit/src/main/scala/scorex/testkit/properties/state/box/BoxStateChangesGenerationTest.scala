package scorex.testkit.properties.state.box

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.Insertion
import scorex.mid.state.BoxMinimalState
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.SemanticallyValidModifierProducer


trait BoxStateChangesGenerationTest[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier,
B <: Box[P],
ST <: BoxMinimalState[P, B, TX, PM, ST]]
  extends BoxStateTests[P, B, TX, PM, ST]
    with TestkitHelpers
    with SemanticallyValidModifierProducer[PM, ST] {


  property("State should be able to generate changes from block and apply them") {
    var state = stateGen.sample.get

    check(checksToMake) { _ =>
      val block = semanticallyValidModifier(state)
      val blockChanges = state.changes(block).get

      blockChanges.toAppend.foreach { case Insertion(b) =>
        state.closedBox(b.id) shouldBe None
      }

      blockChanges.toRemove.foreach { r =>
        state.closedBox(r.boxId).isDefined shouldBe true
      }

      state = state.applyChanges(blockChanges, block.id).get

      blockChanges.toAppend.foreach { case Insertion(b) =>
        state.closedBox(b.id) shouldBe Some(b)
      }
      blockChanges.toRemove.foreach { r =>
        state.closedBox(r.boxId).isDefined shouldBe false
      }
    }
  }
}
