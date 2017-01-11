package examples.hybrid.validation

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import scorex.crypto.hash.CryptographicHash

import scala.util.Try

class SemanticBlockValidator(hash: CryptographicHash) extends BlockValidator {

  def validate(block: HybridPersistentNodeViewModifier): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.brothersCount >= 0)
        require(powBlock.timestamp >= 0)

        //check brothers data
        require(powBlock.brothers.size == powBlock.brothersCount)
        if (powBlock.brothersCount > 0) {
          require(hash(powBlock.brotherBytes) sameElements powBlock.brothersHash)
        }
      case posBlock: PosBlock =>
    }
  }

}
