package examples.curvepos.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import shapeless.{::, HNil}

import scala.util.Try

case class SimpleBlock(override val parentId: BlockId,
                       override val timestamp: Long,
                       generationSignature: GenerationSignature,
                       baseTarget: BaseTarget,
                       generator: PublicKey25519Proposition,
                       txs: Seq[SimpleTransaction])
  extends Block[PublicKey25519Proposition, SimpleTransaction] {

  override def transactions: Option[Seq[SimpleTransaction]] = Some(txs)

  override def id: BlockId = generationSignature

  override type M = SimpleBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[SimpleTransaction] :: HNil

  override val version: Version = 0: Byte

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget :: generator :: txs :: HNil

  override def equals(obj: Any): Boolean = obj match {
    case acc: SimpleBlock => acc.id sameElements this.id
    case _ => false
  }

  override def hashCode(): Int = (BigInt(id) % Int.MaxValue).toInt

}

object SimpleBlock {
  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long
}
