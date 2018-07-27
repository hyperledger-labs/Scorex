package scorex.testkit.properties.state.box

import scorex.core.{PersistentNodeViewModifier, VersionTag, idToVersion}
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
    check(checksToMake) { _ =>
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val state1 = stateGen.sample.get
      val block = semanticallyValidModifier(state1)
      val blockChanges = state1.changes(block).get

      blockChanges.toAppend.foreach { case Insertion(b) =>
        state1.closedBox(b.id) shouldBe None
      }

      blockChanges.toRemove.foreach { r =>
        state1.closedBox(r.boxId).isDefined shouldBe true
      }

      val state2 = state1.applyChanges(blockChanges, idToVersion(block.id)).get

      blockChanges.toAppend.foreach { case Insertion(b) =>
        state2.closedBox(b.id) shouldBe Some(b)
      }
      blockChanges.toRemove.foreach { r =>
        state2.closedBox(r.boxId).isDefined shouldBe false
      }
    }
  }
}
