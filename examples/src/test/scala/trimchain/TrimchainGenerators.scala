package trimchain

import commons.ExamplesCommonGenerators
import examples.commons.SimpleBoxTransaction
import examples.trimchain.core._
import examples.trimchain.modifiers.{BlockHeader, TBlock}
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.ModifierId
import scorex.crypto.authds._

trait TrimchainGenerators extends ExamplesCommonGenerators {

  lazy val stateRootGen: Gen[StateRoot] = genBytesList(Constants.StateRootLength).map(r => StateRoot @@ r)
  lazy val txRootGen: Gen[TransactionsRoot] = genBytesList(Constants.TxRootLength).map(r => TransactionsRoot @@ r)

  val ticketGen: Gen[Ticket] = for {
    minerKey: Array[Byte] <- genBytesList(TicketSerializer.MinerKeySize)
    partialProofs: Seq[ADProof] <- Gen.nonEmptyListOf(nonEmptyBytesGen).map(b => ADProof @@ b)
  } yield Ticket(minerKey, partialProofs)

  val blockHeaderGen: Gen[BlockHeader] = for {
    parentId: ModifierId <- modifierIdGen
    stateRoot: StateRoot <- stateRootGen
    txRoot: TransactionsRoot <- txRootGen
    powNonce: Long <- Arbitrary.arbitrary[Long]
    ticket: Ticket <- ticketGen
  } yield BlockHeader(parentId, stateRoot, txRoot, ticket, powNonce)

  val TBlockGen: Gen[TBlock] = for {
    header: BlockHeader <- blockHeaderGen
    body: Seq[SimpleBoxTransaction] <- Gen.listOf(simpleBoxTransactionGen)
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield TBlock(header, body, timestamp)
}
