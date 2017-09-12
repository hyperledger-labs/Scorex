package scorex.testkit.properties.state

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState

import scala.collection.mutable.ListBuffer

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

  property("Invalid modifier could not be applied to a state") {
    val s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val ver = s.version
      val b = semanticallyInvalidModifier(s)
      val sTry = s.applyModifier(b)
      sTry.isSuccess shouldBe false
      s.version.sameElements(ver) shouldBe true
    }
  }

  //todo: fix
  ignore("Application after rollback is possible") {
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

  property("Application after rollback is possible (within maxRollbackDepth)") {
    var s: ST = stateGen.sample.get
    check(checksToMake) { _ =>
      val buf = new ListBuffer[PM]()
      val maxRollbackDepth = Gen.chooseNum(1, s.maxRollbackDepth).sample.get
      val ver = s.version

      s = (0 until maxRollbackDepth).foldLeft(s) { case (state, _) =>
        val modifier = semanticallyValidModifier(state)
        buf += modifier
        val sTry = state.applyModifier(modifier)
        sTry.toOption shouldBe defined
        sTry.get
      }

      val lastVersion = s.version
      val rollbackTry = s.rollbackTo(ver)
      rollbackTry.toOption shouldBe defined
      s = rollbackTry.get
      s.version.sameElements(ver) shouldBe true

      buf.foreach { m =>
        val sTry = s.applyModifier(m)
        sTry.toOption shouldBe defined
        s = sTry.get
      }

      s.version.sameElements(lastVersion) shouldBe true
    }
  }
}
