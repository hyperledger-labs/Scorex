package hybrid.history

import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.HybridHistory
import examples.hybrid.mining.{MiningSettings, PowMiner}
import hybrid.HybridGenerators
import io.circe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.settings.Settings

class HybridHistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

  var history = HybridHistory.readOrGenerate(settings)

  val genesisBlock = PowBlock(PowMiner.GenesisParentId, PowMiner.GenesisParentId, 1478164225796L, -308545845552064644L,
    0, Array.fill(32)(0: Byte), Seq())

  history = history.append(genesisBlock).get._1

  history.blockById(genesisBlock.id).isDefined shouldBe true

  property("History should be able to add POS blocks") {
    var lastBlockId = genesisBlock.id
    forAll(posBlockGen) { posR =>
      val posBlock = posR.copy(parentId = lastBlockId)
      history = history.append(posBlock).get._1
      history.blockById(posBlock.id).isDefined shouldBe true
      lastBlockId = posBlock.id
    }
  }

}
