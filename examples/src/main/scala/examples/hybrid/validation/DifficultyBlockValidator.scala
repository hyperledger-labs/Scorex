package examples.hybrid.validation

import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HistoryStorage
import examples.hybrid.mining.{HybridMiningSettings, PosForger}
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexEncoding

import scala.util.Try

class DifficultyBlockValidator(settings: HybridMiningSettings, storage: HistoryStorage)
  extends BlockValidator[HybridBlock] with ScorexEncoding {

  def validate(block: HybridBlock): Try[Unit] = block match {
    case b: PowBlock => checkPoWConsensusRules(b)
    case b: PosBlock => checkPoSConsensusRules(b, settings)
  }

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  private def checkPoWConsensusRules(powBlock: PowBlock): Try[Unit] = Try {
    val powDifficulty = storage.getPoWDifficulty(Some(powBlock.prevPosId))
    //check work
    require(powBlock.correctWork(powDifficulty, settings),
      s"Work done is incorrect for block ${encoder.encodeId(powBlock.id)} and difficulty $powDifficulty")

    //some brothers work
    require(powBlock.brothers.forall(_.correctWork(powDifficulty, settings)))

  }

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkPoSConsensusRules(posBlock: PosBlock, miningSettings: HybridMiningSettings): Try[Unit] = Try {
    if (!storage.isGenesis(posBlock)) {
      // TODO: review me - .get
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val parentPoW: PowBlock = storage.modifierById(posBlock.parentId).get.asInstanceOf[PowBlock]
      val hit = PosForger.hit(parentPoW)(posBlock.generatorBox)
      val posDifficulty = storage.getPoSDifficulty(parentPoW.prevPosId)
      val target = (miningSettings.MaxTarget / posDifficulty) * posBlock.generatorBox.value
      require(hit < target, s"$hit < $target failed, $posDifficulty, ")
    }
  }

}
