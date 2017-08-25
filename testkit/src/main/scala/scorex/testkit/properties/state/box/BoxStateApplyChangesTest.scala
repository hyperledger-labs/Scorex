package scorex.testkit.properties.state.box

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.BoxStateChanges
import scorex.mid.state.BoxMinimalState

import scala.util.Random

trait BoxStateApplyChangesTest[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier,
B <: Box[P],
ST <: BoxMinimalState[P, B, TX, PM, ST]] extends BoxStateTests[P, B, TX, PM, ST] {

  val stateChangesGenerator: Gen[BoxStateChanges[P, B]]

  property("State should be able to add a box") {
    forAll(stateChangesGenerator) { c =>
      val newState = state.applyChanges(c, Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte)).get
      c.toAppend.foreach { b =>
        newState.closedBox(b.box.id).isDefined shouldBe true
      }
    }
  }
}
