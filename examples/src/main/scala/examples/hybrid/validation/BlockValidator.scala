package examples.hybrid.validation

import examples.hybrid.blocks.{PosBlock, PowBlock}
import examples.hybrid.mining.MiningConstants
import scorex.crypto.encode.Base58
import scorex.crypto.hash.CryptographicHash

import scala.util.Try

class BlockValidator(settings: MiningConstants, hash: CryptographicHash) {

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  def checkPowConsensusRules(powBlock: PowBlock, powDifficulty: BigInt): Try[Unit] = Try {
    //check work
    require(powBlock.correctWork(powDifficulty, settings),
      s"Work done is incorrent for block ${Base58.encode(powBlock.id)} and difficulty $powDifficulty")

    //check PoW parent id ???
//    modifierById(powBlock.parentId).get

    //some check for header fields
    assert(powBlock.headerValid)

    //check brothers data
    assert(powBlock.brothers.size == powBlock.brothersCount)
    assert(powBlock.brothers.forall(_.correctWork(powDifficulty, settings)))
    if (powBlock.brothersCount > 0) {
      assert(hash(powBlock.brotherBytes) sameElements powBlock.brothersHash)
    }

/*
???
    if (!isGenesis(powBlock)) {
      //check referenced PoS block exists as well
      val posBlock = modifierById(powBlock.prevPosId).get

      //check referenced PoS block points to parent PoW block
      assert(posBlock.parentId sameElements posBlock.parentId, "ref rule broken")
    }
*/
  }

  //PoS consensus rules checks, throws exception if anything wrong
  def checkPoSConsensusRules(posBlock: PosBlock): Try[Unit] = Try {
    //check PoW block exists ???
//    require(modifierById(posBlock.parentId).isDefined)

    //todo: check difficulty

    //todo: check signature

    //todo: check transactions

    //todo: check PoS rules
  }

}
