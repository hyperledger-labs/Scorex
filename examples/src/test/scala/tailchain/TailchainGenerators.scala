package tailchain

import examples.tailchain.core.{PartialProof, Ticket}
import org.scalacheck.{Arbitrary, Gen}
import scorex.testkit.CoreGenerators

trait TailchainGenerators extends CoreGenerators {

  val partialProofGen: Gen[PartialProof] = for {
    id: Array[Byte] <- genBytesList(PartialProof.IdSize)
    rootHash: Array[Byte] <- genBytesList(PartialProof.RootSize)
    proof: Array[Byte] <- nonEmptyBytesGen
  } yield PartialProof(id: Array[Byte], rootHash: Array[Byte], proof: Array[Byte])

  val ticketGen: Gen[Ticket] = for {
    minerKey: Array[Byte] <- genBytesList(Ticket.MinerKeySize)
    nonce: Long <- Arbitrary.arbitrary[Long]
    partialProofs: Seq[PartialProof] <- Gen.nonEmptyListOf(partialProofGen)
  } yield Ticket(minerKey, nonce, partialProofs)

}
