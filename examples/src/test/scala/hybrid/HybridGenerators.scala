package hybrid

import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{PosBlock, PowBlock, PowBlockHeader}
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.SimpleBoxTransaction
import org.scalacheck.{Arbitrary, Gen}
import scorex.ObjectGenerators
import scorex.core.NodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.WalletBox

trait HybridGenerators extends ObjectGenerators {

  lazy val hybridSyncInfoGen: Gen[HybridSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    pos <- genBytesList(NodeViewModifier.ModifierIdSize)
    pow <- genBytesList(NodeViewModifier.ModifierIdSize)
    pows <- Gen.nonEmptyListOf(pow).map(_.take(HybridSyncInfo.MaxLastPowBlocks))
  } yield HybridSyncInfo(answer, pows, pos)

  lazy val signatureGen: Gen[Signature25519] = genBytesList(Signature25519.SignatureSize).map(Signature25519(_))

  lazy val pGen: Gen[(PublicKey25519Proposition, Long)] = for {
    prop <- propositionGen
    long <- positiveLongGen
  } yield (prop, long)

  lazy val privGen: Gen[(PrivateKey25519, Long)] = for {
    prop <- key25519Gen.map(_._1)
    long <- positiveLongGen
  } yield (prop, long)

  lazy val simpleBoxTransactionGen: Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from: IndexedSeq[(PrivateKey25519, Long)] <- Gen.nonEmptyListOf(privGen).map(_.toIndexedSeq)
    to: IndexedSeq[(PublicKey25519Proposition, Long)] <- Gen.nonEmptyListOf(pGen).map(_.toIndexedSeq)
  } yield SimpleBoxTransaction(from, to, fee, timestamp)

  lazy val blockIdGen: Gen[BlockId] = genBytesList(Block.BlockIdLength)

  lazy val blockIdsGen: Gen[Seq[BlockId]] = Gen.listOf(blockIdGen)

  lazy val nonEmptyBlockIdsGen: Gen[Seq[BlockId]] = Gen.nonEmptyListOf(blockIdGen)

  lazy val posBlockGen: Gen[PosBlock] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    txs: Seq[SimpleBoxTransaction] <- Gen.listOf(simpleBoxTransactionGen)
    generator: PublicKey25519Proposition <- propositionGen
    signature: Signature25519 <- signatureGen
  } yield PosBlock(parentId, timestamp, txs, generator, signature)


  lazy val powHeaderGen: Gen[PowBlockHeader] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    prevPosId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    brothersCount: Byte <- positiveByteGen
    brothersHash: Array[Byte] <- genBytesList(FastCryptographicHash.DigestSize)
  } yield new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash)

  lazy val powBlockGen: Gen[PowBlock] = for {
    parentId: BlockId <- genBytesList(Block.BlockIdLength)
    prevPosId: BlockId <- genBytesList(Block.BlockIdLength)
    timestamp: Long <- positiveLongGen
    nonce: Long <- positiveLongGen
    brothersCount: Byte <- positiveByteGen
    brothersHash: Array[Byte] <- genBytesList(FastCryptographicHash.DigestSize)
    brothers <- Gen.listOfN(brothersCount, powHeaderGen)
  } yield new PowBlock(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, brothers)


  lazy val noncedBoxGen: Gen[PublicKey25519NoncedBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield PublicKey25519NoncedBox(proposition, nonce, value)

  lazy val walletBoxGen: Gen[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = for {
    createdAt <- positiveLongGen
    txId <- genBytesList(NodeViewModifier.ModifierIdSize)
    box: PublicKey25519NoncedBox <- noncedBoxGen
  } yield WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, createdAt)(PublicKey25519NoncedBoxSerializer)

}
