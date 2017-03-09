package examples.hybrid.validation

import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import scorex.core.block.BlockValidator
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.hash.CryptographicHash
import scorex.crypto.signatures.Curve25519

import scala.util.Try

class SemanticBlockValidator(hash: CryptographicHash) extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = Try {
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
        require(posBlock.timestamp >= 0)
        require(PosBlock.signatureValid(posBlock))
    }
  }

}
