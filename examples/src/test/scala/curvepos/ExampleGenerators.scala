package curvepos

import examples.curvepos.transaction.{SimpleBlock, SimplePayment, SimpleTransaction}
import org.scalacheck.{Arbitrary, Gen}
import scorex.ObjectGenerators
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait ExampleGenerators extends ObjectGenerators {
  val paymentGen: Gen[SimplePayment] = for {
    sender: PublicKey25519Proposition <- propositionGen
    recipient: PublicKey25519Proposition <- propositionGen
    amount: Long <- Arbitrary.arbitrary[Long]
    fee: Long <- Arbitrary.arbitrary[Long]
    nonce: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield SimplePayment(sender, recipient, amount, fee, nonce, timestamp)


  val blockGenerator: Gen[SimpleBlock] = for {
    parentId: BlockId <- genBoundedBytes(Block.BlockIdLength, Block.BlockIdLength)
    timestamp: Long <- Arbitrary.arbitrary[Long]
    baseTarget: Long <- Arbitrary.arbitrary[Long]
    generationSignature <- genBoundedBytes(64, 64)
    generator: PublicKey25519Proposition <- propositionGen
    txs: Seq[SimpleTransaction] <- Gen.listOf(paymentGen)
  } yield new SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)

}
