package tailchain

import commons.ExamplesCommonGenerators
import examples.commons.SimpleBoxTransaction
import examples.tailchain.core._
import examples.tailchain.modifiers.{BlockHeader, TBlock}
import org.scalacheck.{Arbitrary, Gen}

trait TailchainGenerators extends ExamplesCommonGenerators {
  val ticketGen: Gen[Ticket] = for {
    minerKey: Array[Byte] <- genBytesList(TicketSerializer.MinerKeySize)
    partialProofs: Seq[Array[Byte]] <- Gen.nonEmptyListOf(nonEmptyBytesGen)
  } yield Ticket(minerKey, partialProofs)

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
