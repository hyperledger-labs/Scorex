package tailchain

import examples.tailchain.core._
import examples.tailchain.modifiers.BlockHeader
import org.scalacheck.{Arbitrary, Gen}
import scorex.testkit.CoreGenerators

trait TailchainGenerators extends CoreGenerators {

  val partialProofGen: Gen[PartialProof] = for {
    id: Array[Byte] <- genBytesList(PartialProofSerializer.IdSize)
    rootHash: Array[Byte] <- genBytesList(PartialProofSerializer.RootSize)
    proof: Array[Byte] <- nonEmptyBytesGen
  } yield PartialProof(id: Array[Byte], rootHash: Array[Byte], proof: Array[Byte])

  val ticketGen: Gen[Ticket] = for {
    minerKey: Array[Byte] <- genBytesList(TicketSerializer.MinerKeySize)
    nonce: Long <- Arbitrary.arbitrary[Long]
    partialProofs: Seq[PartialProof] <- Gen.nonEmptyListOf(partialProofGen)
  } yield Ticket(minerKey, nonce, partialProofs)

  val blockHeaderGen: Gen[BlockHeader] = for {
    parentId: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    stateRoot: Array[Byte] <- genBytesList(Constants.StateRootLength)
    txRoot: Array[Byte] <- genBytesList(Constants.TxRootLength)
    powNonce: Long <- Arbitrary.arbitrary[Long]
    ticket: Ticket <- ticketGen
  } yield BlockHeader(parentId, stateRoot, txRoot, ticket, powNonce)

}
