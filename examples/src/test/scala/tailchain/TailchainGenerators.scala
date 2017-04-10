package tailchain

import commons.ExamplesCommonGenerators
import examples.commons.SimpleBoxTransaction
import examples.tailchain.core._
import examples.tailchain.modifiers.{BlockHeader, TBlock}
import org.scalacheck.{Arbitrary, Gen}

trait TailchainGenerators extends ExamplesCommonGenerators {

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

  val TBlockGen: Gen[TBlock] = for {
    header: BlockHeader <- blockHeaderGen
    body: Seq[SimpleBoxTransaction] <- Gen.listOf(simpleBoxTransactionGen)
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield TBlock(header, body, timestamp)

}
