package scorex.testkit.properties.state

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState

trait StateApplicationTest[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] extends StateTests[PM, ST] {

  property("Valid modifier is being applied successfully to state") {
    var s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
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
      sTry.isSuccess shouldBe true
      s = sTry.get
      s.version.sameElements(ver) shouldBe false
      s.applyModifier(b).isSuccess shouldBe false
    }
  }

  property("Invalid modifier could not be applied state") {
    val s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyInvalidModifier(s)
      val sTry = s.applyModifier(b)
      sTry.isSuccess shouldBe false
      s.version.sameElements(ver) shouldBe true
    }
  }

  property("Application after rollback is possible") {
    var s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyValidModifier(s)
      val sTry = s.applyModifier(b)
      sTry.isSuccess shouldBe true
      s = sTry.get
      s.version.sameElements(ver) shouldBe false
      val ver2 = s.version

      s = s.rollbackTo(ver).get
      s.version.sameElements(ver) shouldBe true

      val sTry2 = s.applyModifier(b)
      sTry2.isSuccess shouldBe true
      s = sTry2.get
      s.version.sameElements(ver) shouldBe false
      s.version.sameElements(ver2) shouldBe true
    }
  }
}
