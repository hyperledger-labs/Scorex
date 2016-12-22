package hybrid.history

import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.HybridHistory
import examples.hybrid.mining.MiningConstants
import hybrid.HybridGenerators
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.NodeViewModifier.ModifierId
import scorex.crypto.encode.Base58

import scala.util.Random


class HybridHistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val constants = new MiningConstants {
    override lazy val Difficulty: BigInt = 1
  }
  var history = HybridHistory.readOrGenerate(s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}", None, constants)
  val genesisBlock = PowBlock(constants.GenesisParentId, constants.GenesisParentId, 1478164225796L, -308545845552064644L,
    0, Array.fill(32)(0: Byte), Seq())
  history = history.append(genesisBlock).get._1
  history.modifierById(genesisBlock.id).isDefined shouldBe true

  //Generate chain
  property("Block application and HybridHistory.continuationIds") {
    var ids: Seq[ModifierId] = Seq()
    forAll(posBlockGen, powBlockGen) { (posR, powR) =>
      if (history.powHeight <= HybridHistory.DifficultyRecalcPeriod) {
        val posBlock = posR.copy(parentId = history.bestPowId)
        history = history.append(posBlock).get._1

        var powBlock = powR.copy(parentId = history.bestPowId, prevPosId = history.bestPosId, brothers = Seq(),
          brothersCount = 0)
        while (!powBlock.correctWork(history.powDifficulty, constants)) {
          powBlock = powBlock.copy(nonce = Random.nextLong())
        }
        history = history.append(powBlock).get._1

        history.modifierById(posBlock.id).isDefined shouldBe true
        history.modifierById(powBlock.id).isDefined shouldBe true
        ids = ids ++ Seq(posBlock.id, powBlock.id)
      }
    }

    val startFrom = ids.head

    history.continuationIds(Seq((2.toByte, startFrom)), ids.length).get.map(_._2).map(Base58.encode) shouldEqual ids.map(Base58.encode)

    ids.length shouldBe HybridHistory.DifficultyRecalcPeriod * 2

    //continuationIds with limit
    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((2.toByte, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map(a => (2.toByte, a))
      val restIds = ids.zipWithIndex.filter { case (datum, index) => index >= startIndex }.map(_._1).map(Base58.encode)

      history.continuationIds(startFrom, ids.length).get.map(_._2).map(Base58.encode) shouldEqual restIds
      history.continuationIds(startList, ids.length).get.map(_._2).map(Base58.encode) shouldEqual restIds

      val limit = 5
      history.continuationIds(startList, limit) match {
        case None => restIds.length should be >= limit
        case Some(l) => l.length shouldBe restIds.length
      }
    }
  }
}
