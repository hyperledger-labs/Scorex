package hybrid.history

import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.HybridHistory
import examples.hybrid.mining.{MiningConstants, PowMiner}
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

    forAll(Gen.choose(0, ids.length - 2)) { startIndex: Int =>
      val startFrom = ids(startIndex)
      val restIds = ids.zipWithIndex.filter { case (datum, index) => index >= startIndex }.map(_._1).map(Base58.encode)

      history.continuationIds(Seq((2.toByte, startFrom)), ids.length).get.map(_._2).map(Base58.encode) shouldEqual restIds
    }
  }

  property("commonBlockThenSuffixes finds correct suffixes") {

    forAll(nonEmptyBlockIdsGen, blockIdsGen, blockIdsGen) { case (prefix, suffix1, suffix2) =>
      val (winnerChain, loserChain) = (prefix ++ suffix1, prefix ++ suffix2)

      whenever(
        !suffix1.headOption.getOrElse(Array()).sameElements(suffix2.headOption.getOrElse(Array()))
          && suffix1.length > suffix2.length
          && prefix.length >= 2
          && winnerChain.forall(blockId => winnerChain.count(_.sameElements(blockId)) == 1)
          && loserChain.forall(blockId => loserChain.count(_.sameElements(blockId)) == 1)
      ) {
        val (s1, s2) = HybridHistory.commonBlockThenSuffixes(winnerChain, loserChain)
        s1.length shouldBe suffix1.length + 1
        s2.length shouldBe suffix2.length + 1
        s1.tail.headOption.getOrElse(Array()).sameElements(s2.tail.headOption.getOrElse(Array())) shouldBe false
        s1.headOption.getOrElse(Array()).sameElements(s2.headOption.getOrElse(Array())) shouldBe true
      }
    }
  }
}
