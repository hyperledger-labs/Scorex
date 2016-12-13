package hybrid.history

import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.HybridHistory
import examples.hybrid.mining.PowMiner
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scala.util.Random


class HybridHistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {


  property("History should be able to add POS blocks") {
    forAll(posBlockGen) { posR =>
      var history = HybridHistory.readOrGenerate("/tmp/"+s"scorextest-${Random.nextInt(10000000)}", None)

      val genesisBlock = PowBlock(PowMiner.GenesisParentId, PowMiner.GenesisParentId, 1478164225796L, -308545845552064644L,
        0, Array.fill(32)(0: Byte), Seq())

      history = history.append(genesisBlock).get._1

      history.modifierById(genesisBlock.id).isDefined shouldBe true

      val posBlock = posR.copy(parentId = genesisBlock.id)
      history = history.append(posBlock).get._1
      history.modifierById(posBlock.id).isDefined shouldBe true
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
        s2.length shouldBe  suffix2.length + 1
        s1.tail.headOption.getOrElse(Array()).sameElements(s2.tail.headOption.getOrElse(Array())) shouldBe false
        s1.headOption.getOrElse(Array()).sameElements(s2.headOption.getOrElse(Array())) shouldBe true
      }
    }
  }
}
