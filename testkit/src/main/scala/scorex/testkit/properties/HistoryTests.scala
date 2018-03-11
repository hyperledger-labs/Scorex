package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, Valid, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.SyntacticallyTargetedModifierProducer


trait HistoryTests[P <: Proposition,
                              TX <: Transaction[P],
                              PM <: PersistentNodeViewModifier,
                              SI <: SyncInfo,
                              HT <: History[PM, SI, HT]]
  extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with PropertyChecks
    with ScorexLogging
    with TestkitHelpers
    with SyntacticallyTargetedModifierProducer[PM, SI, HT] {

  val historyGen: Gen[HT]

  lazy val generatorWithValidModifier: Gen[(HT, PM)] = historyGen.map { h => (h, syntacticallyValidModifier(h))}
  lazy val generatorWithInvalidModifier: Gen[(HT, PM)] = historyGen.map { h => (h, syntacticallyInvalidModifier(h))}

  private def propertyNameGenerator(propName: String): String = s"HistoryTests: $propName"

  property(propertyNameGenerator("applicable with valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) => h.applicable(m) shouldBe true}
  }

  property(propertyNameGenerator("append valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) => h.append(m).isSuccess shouldBe true }
  }

  property(propertyNameGenerator("contain valid modifier after appending")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.contains(m) shouldBe true
    }
  }

  property(propertyNameGenerator("find valid modifier after appending by modifierId")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.modifierById(m.id) shouldBe defined
    }
  }

  property(propertyNameGenerator("report semantically validation after appending valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.reportSemanticValidity(m, true, m.id)
      h.isSemanticallyValid(m.id) shouldBe Valid
    }
  }

  property(propertyNameGenerator("not applicable with invalid modifier")) {
    forAll(generatorWithInvalidModifier) { case (h, m) => h.applicable(m) shouldBe false}
  }

  property(propertyNameGenerator("not append invalid modifier")) {
    forAll(generatorWithInvalidModifier) { case (h, m) => h.append(m).isSuccess shouldBe false }
  }

  property(propertyNameGenerator("not contain invalid modifier after appending")) {
    forAll(generatorWithInvalidModifier) { case (h, m) =>
      h.append(m)
      h.contains(m) shouldBe false
    }
  }

  property(propertyNameGenerator("not finds valid modifier after appending by modifierId")) {
    forAll(generatorWithInvalidModifier) { case (h, m) =>
      h.append(m)
      h.modifierById(m.id) shouldBe None
    }
  }
}