package scorex.testkit.properties.state.box

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{BoxStateChanges, Removal}
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
      val block = semanticallyValidModifier(state)
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
