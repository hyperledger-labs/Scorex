package examples.curvepos.transaction

import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
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

  override lazy val id: BlockId = FastCryptographicHash(generationSignature)

  override lazy val version: Version = 0: Byte

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget ::
    generator :: txs :: HNil

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> generator.toString.asJson,
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