package hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.PosBlock
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.SimpleBoxTransaction
import org.scalacheck.{Arbitrary, Gen}
import scorex.ObjectGenerators
import scorex.core.NodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.wallet.WalletBox

trait HybridGenerators extends ObjectGenerators {

  lazy val hybridSyncInfoGen: Gen[HybridSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    pos <- genBytesList(NodeViewModifier.ModifierIdSize)
    pow <- genBytesList(NodeViewModifier.ModifierIdSize)
  } yield HybridSyncInfo(answer, pow, pos)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val pGen: Gen[(PublicKey25519Proposition, Long)] = for {
    prop <- propositionGen
    long <- positiveLongGen
  } yield (prop, long)

  lazy val simpleBoxTransactionGen: Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    signatures: IndexedSeq[Signature25519] <- Gen.listOf(signatureGen).map(_.toIndexedSeq)
    from: IndexedSeq[(PublicKey25519Proposition, Long)] <- Gen.listOf(pGen).map(_.toIndexedSeq)
    to: IndexedSeq[(PublicKey25519Proposition, Long)] <- Gen.listOf(pGen).map(_.toIndexedSeq)
  } yield SimpleBoxTransaction(from, to, signatures, fee, timestamp)

  lazy val posBlockGen: Gen[PosBlock] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransaction] <- Gen.listOf(simpleBoxTransactionGen)
    generator: PublicKey25519Proposition <- propositionGen
    signature: Signature25519 <- signatureGen
  } yield PosBlock(parentId, timestamp, txs, generator, signature)

  lazy val noncedBoxGen: Gen[PublicKey25519NoncedBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield PublicKey25519NoncedBox(proposition, nonce, value)

  lazy val walletBoxGen: Gen[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = for {
    createdAt <- positiveLongGen
    txId <- genBytesList(NodeViewModifier.ModifierIdSize)
    box: PublicKey25519NoncedBox <- noncedBoxGen
  } yield WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, createdAt)

}
