package curvepos

import examples.curvepos._
import examples.curvepos.transaction.{SimpleBlock, SimplePayment, SimpleTransaction, SimpleWallet}
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.ModifierId
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.testkit.generators.CoreGenerators

trait ExampleGenerators extends CoreGenerators {
  val wallet = SimpleWallet()
  val genesisAcc = wallet.secrets.head

  def generationSignatureGen: Gen[GenerationSignature] = genBoundedBytes(64, 64).map(s => GenerationSignature @@ s)

  def baseTargetGen: Gen[BaseTarget] = Arbitrary.arbitrary[Long].map(s => BaseTarget @@ s)

  lazy val paymentGen: Gen[SimplePayment] = for {
    sender: PublicKey25519Proposition <- propositionGen
    recipient: PublicKey25519Proposition <- propositionGen
    amount: Long <- positiveLongGen
    fee: Long <- positiveLongGen
    nonce: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield SimplePayment(sender, recipient, amount, fee, nonce, timestamp)


  lazy val blockGenerator: Gen[SimpleBlock] = for {
    parentId: BlockId <- ModifierId @@ genBoundedBytes(Block.BlockIdLength, Block.BlockIdLength)
    timestamp: Long <- Arbitrary.arbitrary[Long]
    baseTarget: BaseTarget <- baseTargetGen
    generationSignature <- generationSignatureGen
    generator: PublicKey25519Proposition <- propositionGen
    txs: Seq[SimpleTransaction] <- Gen.listOf(paymentGen)
  } yield new SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)

  val genesisBlock: SimpleBlock = {

    val IntitialBasetarget = BaseTarget @@ 153722867L
    val generator = PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(0: Byte))
    val toInclude: Seq[SimpleTransaction] = Seq(SimplePayment(genesisAcc.publicImage, genesisAcc.publicImage, Long.MaxValue, 0, 1, 0))

    SimpleBlock(ModifierId @@ Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
      0L,
      GenerationSignature @@ Array.fill(SimpleBlock.SignatureLength)(0: Byte),
      IntitialBasetarget,
      generator,
      toInclude)
  }
}
