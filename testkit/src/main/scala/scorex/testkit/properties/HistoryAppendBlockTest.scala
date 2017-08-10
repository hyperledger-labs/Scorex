package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.testkit.TestkitHelpers

trait HistoryAppendBlockTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks
  with ScorexLogging with TestkitHelpers {
  val history: HT

  def genValidModifier(history: HT, mempoolTransactionFetchOption: Boolean, noOfTransactionsFromMempool : Int): PM

  property("Appended block is in history") {
    var h: HT = history
    check { _ =>
      val b = genValidModifier(h, mempoolTransactionFetchOption = false, 0)
      h.modifierById(b.id).isDefined shouldBe false
      h = h.append(b).get._1
      h.modifierById(b.id).isDefined shouldBe true
    }
  }
}
