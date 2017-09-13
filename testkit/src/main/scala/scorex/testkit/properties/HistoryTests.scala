package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, ModifierSemanticValidity, SyncInfo}
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

  val validModifierCase: HT => Unit = { history =>
    var h = history
    val b = syntacticallyValidModifier(h)
    h.applicable(b) shouldBe true
    h.modifierById(b.id) shouldBe None

    h = h.append(b).get._1

    h.modifierById(b.id) shouldBe Some(b)
    h.contains(b) shouldBe true
    h.reportSemanticValidity(b, true, b.id)
    h.isSemanticallyValid(b.id) shouldBe ModifierSemanticValidity.Valid
  }

  val invalidModifierCase: HT => Unit = {history =>
    val b = syntacticallyInvalidModifier(history)
    history.applicable(b) shouldBe false
    history.modifierById(b.id) shouldBe None

    history.append(b).isSuccess shouldBe false

    history.modifierById(b.id) shouldBe None
    history.contains(b) shouldBe false
    history.isSemanticallyValid(b.id) shouldBe ModifierSemanticValidity.Absent
  }

  property("Valid block is being appended successfully to the history") {
    forAll(historyGen){ validModifierCase }
  }

  property("Invalid block is NOT being appended successfully to the history") {
    forAll(historyGen) { invalidModifierCase }
  }
}