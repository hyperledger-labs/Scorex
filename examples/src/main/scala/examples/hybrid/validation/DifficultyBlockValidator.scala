package examples.hybrid.validation

import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HistoryStorage
import examples.hybrid.mining.MiningConstants
import scorex.core.block.BlockValidator
import scorex.crypto.encode.Base58
import scorex.crypto.hash.CryptographicHash

import scala.util.Try

class DifficultyBlockValidator(settings: MiningConstants,  storage: HistoryStorage)
  extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = block match {
    case b: PowBlock => checkPoWConsensusRules(b)
    case b: PosBlock => checkPoSConsensusRules(b)
  }

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  private def checkPoWConsensusRules(powBlock: PowBlock): Try[Unit] = Try {
    val powDifficulty = storage.getPoWDifficulty(Some(powBlock.prevPosId))
    //check work
    require(powBlock.correctWork(powDifficulty, settings),
      s"Work done is incorrect for block ${Base58.encode(powBlock.id)} and difficulty $powDifficulty")

    //some brothers work
    require(powBlock.brothers.forall(_.correctWork(powDifficulty, settings)))

  }

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkPoSConsensusRules(posBlock: PosBlock): Try[Unit] = Try {

    //todo: check difficulty

    //todo: check signature

    //todo: check transactions

    //todo: check PoS rules
  }

}
