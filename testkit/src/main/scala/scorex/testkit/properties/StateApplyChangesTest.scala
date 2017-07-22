package scorex.testkit.properties

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.transaction.state.BoxStateChanges

import scala.util.Random

trait StateApplyChangesTest[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: BoxMinimalState[P, B, TX, PM, ST]] extends StateTests[P, TX, PM, B, ST] {

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
