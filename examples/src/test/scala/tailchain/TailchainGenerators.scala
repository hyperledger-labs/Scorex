package tailchain

import examples.tailchain.core.PartialProof
import org.scalacheck.Gen
import scorex.testkit.CoreGenerators

trait TailchainGenerators extends CoreGenerators {

  val partialProofGen: Gen[PartialProof] = for {
    id: Array[Byte] <- genBytesList(PartialProof.IdSize)
    rootHash: Array[Byte] <- genBytesList(PartialProof.RootSize)
    proof: Array[Byte] <- nonEmptyBytesGen
  } yield PartialProof(id: Array[Byte], rootHash: Array[Byte], proof: Array[Byte])

}
