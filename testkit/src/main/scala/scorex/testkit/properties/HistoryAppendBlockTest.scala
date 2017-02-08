package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

trait HistoryAppendBlockTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks
  with ScorexLogging {
  val history: HT

  def genValidModifier(history: HT): PM

  property("Appended block is in history") {
    var h: HT = history
    (0 until 100) foreach { _ =>
      val b = genValidModifier(h)
      h.modifierById(b.id).isDefined shouldBe false
      h = h.append(b).get._1
      h.modifierById(b.id).isDefined shouldBe true
    }
  }


//  val validBlockGenerator: Gen[PM]
//  var h: HT = history
//  property("Appended block is in history") {
//    forAll(validBlockGenerator) { b: PM =>
//      h.modifierById(b.id).isDefined shouldBe false
//      h = h.append(b).get._1
//      h.modifierById(b.id).isDefined shouldBe true
//    }
//  }


}
