package curvepos

import examples.curvepos.transaction.SimpleBlock.GenerationSignature
import examples.curvepos.transaction.{SimpleBlock, SimpleBlockCompanion, SimplePayment, SimpleTransaction}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

/**
  * Created by pozharko on 02.10.16.
  */
class SimpleBlockTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ObjectGenerators {

  val transactionGen: Gen[SimpleTransaction] = for {
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
    generationSignature: GenerationSignature <- genBoundedBytes(64, 64)
    generator: PublicKey25519Proposition <- propositionGen
    txs: Seq[SimpleTransaction] <- Gen.listOf(transactionGen)
  } yield new SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)

  property("Block serialization") {
    forAll(blockGenerator) { b: SimpleBlock =>
      val recovered = SimpleBlockCompanion.parse(SimpleBlockCompanion.bytes(b)).get
      SimpleBlockCompanion.bytes(b) shouldEqual SimpleBlockCompanion.bytes(recovered)
    }
  }

}
