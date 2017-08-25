package scorex.testkit.properties.state

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState

trait StateApplicationTest[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] extends StateTests[PM, ST] {

  val checksToMake = 10

  property("Valid block is being applied successfully to state") {
    var s: ST = state
    check(checksToMake) { _ =>
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
      sTry.isSuccess shouldBe true
      s = sTry.get
    }
  }

  property("Double application is not possible") {
    var s: ST = state
    check(checksToMake) { _ =>
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
      println(sTry.get)
      sTry.isSuccess shouldBe true
      s = sTry.get
      s.applyModifier(b).isSuccess shouldBe false
    }
  }
}
