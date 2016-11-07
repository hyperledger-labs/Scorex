package examples.curvepos.transaction

import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import shapeless.{::, HNil}

case class SimpleBlock(override val parentId: BlockId,
                       override val timestamp: Block.Timestamp,
                       generationSignature: GenerationSignature,
                       baseTarget: BaseTarget,
                       generator: PublicKey25519Proposition,
                       txs: Seq[SimpleTransaction])
  extends Block[PublicKey25519Proposition, SimpleTransaction] {

  override type M = SimpleBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[SimpleTransaction] :: HNil

  override lazy val modifierTypeId: Byte = SimpleBlock.ModifierTypeId

  override lazy val transactions: Option[Seq[SimpleTransaction]] = Some(txs)

  override lazy val id: BlockId = ???

  override lazy val version: Version = 0: Byte

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget ::
    generator :: txs :: HNil

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson

  override def equals(obj: Any): Boolean = obj match {
    case acc: SimpleBlock => acc.id sameElements this.id
    case _ => false
  }

  override def hashCode(): Int = (BigInt(id) % Int.MaxValue).toInt

}

object SimpleBlock {
  val ModifierTypeId = 1: Byte

  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long
}

/*
object SimpleBlockCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  import SimplePaymentCompanion.TransactionLength

  def messageToSign(block: SimpleBlock): Array[Byte] = {
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntBytes) { case (bytes, tx) => bytes ++ tx.bytes }
    }
  }

  override def bytes(block: SimpleBlock): Array[Byte] = {
    //TODO use messageToSign
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      block.generationSignature ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntBytes) { case (bytes, tx) => bytes ++ tx.bytes }
    }
  }

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(Block.BlockIdLength, Block.BlockIdLength + 8))
    val version = bytes.slice(Block.BlockIdLength + 8, Block.BlockIdLength + 9).head
    val s0 = Block.BlockIdLength + 9
    val generationSignature = bytes.slice(s0, s0 + SimpleBlock.SignatureLength)
    val baseTarget = Longs.fromByteArray(bytes.slice(s0 + SimpleBlock.SignatureLength, s0 + SimpleBlock.SignatureLength + 8))
    val s1 = s0 + SimpleBlock.SignatureLength + 8
    val generator = PublicKey25519Proposition(bytes.slice(s1, s1 + 32))
    val cnt = Ints.fromByteArray(bytes.slice(s1 + 32, s1 + 36))
    val s2 = s1 + 36
    val txs = (0 until cnt) map { i =>
      val bt = bytes.slice(s2 + TransactionLength * i, s2 + TransactionLength * (i + 1))
      SimplePaymentCompanion.parse(bt).get
    }
    SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)
  }

}
*/
