package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.encode.Base58

class HistorySanity[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {
  type HT = History[P, TX, PM, SI, _ <: History[P, TX, PM, SI, _]]

  def appendedBlockIsInHistory(history: HT, blockGen: Gen[PM]): Unit = {
    forAll(blockGen) { b: PM =>
      println(s"!! ${Base58.encode(b.id)}")
      history.modifierById(b.id).isDefined shouldBe false
      val updatedHistory = history.append(b).get._1
      updatedHistory.modifierById(b.id).isDefined shouldBe true
    }
  }

}
