package curvepos

import examples.curvepos.transaction.{SimpleBlock, SimplePayment, SimpleTransaction, SimpleWallet}
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519
import scorex.testkit.CoreGenerators

trait ExampleGenerators extends CoreGenerators {
  val wallet = SimpleWallet()
  val genesisAcc = wallet.secrets.head

  lazy val paymentGen: Gen[SimplePayment] = for {
    sender: PublicKey25519Proposition <- propositionGen
    recipient: PublicKey25519Proposition <- propositionGen
    amount: Long <- positiveLongGen
    fee: Long <- positiveLongGen
    nonce: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield SimplePayment(sender, recipient, amount, fee, nonce, timestamp)


  lazy val blockGenerator: Gen[SimpleBlock] = for {
    parentId: BlockId <- genBoundedBytes(Block.BlockIdLength, Block.BlockIdLength)
    timestamp: Long <- Arbitrary.arbitrary[Long]
    baseTarget: Long <- Arbitrary.arbitrary[Long]
    generationSignature <- genBoundedBytes(64, 64)
    generator: PublicKey25519Proposition <- propositionGen
    txs: Seq[SimpleTransaction] <- Gen.listOf(paymentGen)
  } yield new SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)

  val genesisBlock: SimpleBlock = {

    val IntitialBasetarget = 153722867L
    val generator = PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte))
    val toInclude: Seq[SimpleTransaction] = Seq(SimplePayment(genesisAcc.publicImage, genesisAcc.publicImage, Long.MaxValue, 0, 1, 0))

    SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
      0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), IntitialBasetarget, generator, toInclude)

  }
}
