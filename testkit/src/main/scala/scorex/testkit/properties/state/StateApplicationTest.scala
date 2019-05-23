package scorex.testkit.properties.state

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState

import scala.collection.mutable.ListBuffer

trait StateApplicationTest[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] extends StateTests[PM, ST] {

  lazy val stateGenWithValidModifier: Gen[(ST, PM)] = stateGen.map { s => (s, semanticallyValidModifier(s))}
  lazy val stateGenWithInvalidModifier: Gen[(ST, PM)] = stateGen.map { s => (s, semanticallyInvalidModifier(s))}

  private def propertyNameGenerator(propName: String): String = s"StateTests: $propName"

  property(propertyNameGenerator("apply modifier")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      val sTry = s.applyModifier(m)
        sTry.isSuccess shouldBe true
      sTry.get.version == ver shouldBe false
    }
  }

  property(propertyNameGenerator("do not apply same valid modifier twice")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      val sTry = s.applyModifier(m)
      sTry.isSuccess shouldBe true
      val s2 = sTry.get
      s2.version == ver shouldBe false
      s2.applyModifier(m).isSuccess shouldBe false
    }
  }

  property(propertyNameGenerator("do not apply invalid modifier")) {
    forAll(stateGenWithInvalidModifier) { case (s, m) =>
      val sTry = s.applyModifier(m)
      sTry.isSuccess shouldBe false
    }
  }

  property(propertyNameGenerator("apply valid modifier after rollback")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      val sTry = s.applyModifier(m)
      sTry.isSuccess shouldBe true
      val s2 = sTry.get
      s2.version == ver shouldBe false
      val ver2 = s2.version

      val s3 = s2.rollbackTo(ver).get
      s3.version == ver shouldBe true

      val sTry2 = s3.applyModifier(m)
      sTry2.isSuccess shouldBe true
      val s4 = sTry2.get
      s4.version == ver shouldBe false
      s4.version == ver2 shouldBe true
    }
  }

  property(propertyNameGenerator("application after rollback is possible (within maxRollbackDepth)")) {
    forAll(stateGen) { s =>
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val rollbackDepth = Gen.chooseNum(1, s.maxRollbackDepth).sample.get
      val buf = new ListBuffer[PM]()
      val ver = s.version

      val s2 = (0 until rollbackDepth).foldLeft(s) { case (state, _) =>
        val modifier = semanticallyValidModifier(state)
        buf += modifier
        val sTry = state.applyModifier(modifier)
        sTry shouldBe 'success
        sTry.get
      }

      val lastVersion = s2.version
      val rollbackTry = s2.rollbackTo(ver)
      rollbackTry.toOption shouldBe defined
      val s3 = rollbackTry.get
      s3.version == ver shouldBe true

      val s4 = buf.foldLeft(s3) { case (state, m) =>
        val sTry = state.applyModifier(m)
        sTry shouldBe 'success
        sTry.get
      }

      s4.version == lastVersion shouldBe true
    }
  }
}
