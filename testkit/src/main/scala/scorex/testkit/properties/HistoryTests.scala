package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
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

  val history: HT

  property("Valid block is being appended successfully to the history") {
    var h: HT = history
    check { _ =>
      val b = syntacticallyValidModifier(h)
      h.modifierById(b.id) shouldBe None
      h = h.append(b).get._1
      h.modifierById(b.id) shouldBe Some(b)
      h.contains(b) shouldBe true
    }
  }

  property("Invalid block is NOT being appended successfully to the history") {
    val h: HT = history
    check { _ =>
      val b = syntacticallyInvalidModifier(h)
      h.modifierById(b.id) shouldBe None
      h.append(b).isSuccess shouldBe false
      h.modifierById(b.id) shouldBe None
      h.contains(b) shouldBe false
    }
  }
}