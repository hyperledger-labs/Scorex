package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

class HistorySanity[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks
  with ScorexLogging {
  type HT = History[P, TX, PM, SI, _ <: History[P, TX, PM, SI, _]]

  def appendedBlockIsInHistory(history: HT, blockGen: Gen[PM]): Unit = {
    forAll(blockGen) { b: PM =>
      history.modifierById(b.id).isDefined shouldBe false
      history.append(b) match {
        case Success((updatedHistory, _)) => updatedHistory.modifierById(b.id).isDefined shouldBe true
        case Failure(e) => log.debug(s"Incorrect block $b from generator", e)
      }
    }
  }

}
