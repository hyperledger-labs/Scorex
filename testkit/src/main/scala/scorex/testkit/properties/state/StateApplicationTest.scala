package scorex.testkit.properties.state

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState

trait StateApplicationTest[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] extends StateTests[PM, ST] {

  property("Valid block is being applied successfully to state") {
    var s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
      println(sTry.get)
      sTry.isSuccess shouldBe true
      s = sTry.get
      s.version.sameElements(ver) shouldBe false
    }
  }

  property("Double application is not possible") {
    var s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
      println(sTry.get)
      sTry.isSuccess shouldBe true
      s = sTry.get
      s.version.sameElements(ver) shouldBe false
      s.applyModifier(b).isSuccess shouldBe false
    }
  }
}
