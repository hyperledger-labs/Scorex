package examples.curvepos.transaction

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.SimpleBlock._
import io.circe.Json
import io.circe.syntax._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.NodeViewModifierCompanion
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

  override def companion: NodeViewModifierCompanion[SimpleBlock] = SimpleBlockCompanion

  override def id: BlockId =
    FastCryptographicHash(txs.map(_.messageToSign).reduce(_ ++ _) ++ generator.bytes)

  override type M = SimpleBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[SimpleTransaction] :: HNil

  override val version: Version = 0: Byte

  override def json: Json = Map(
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget :: generator :: txs :: HNil
}

object SimpleBlock {
  type GenerationSignature = Array[Byte]

  type BaseTarget = Long
}

object SimpleBlockCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  override def bytes(block: SimpleBlock): Array[ModifierTypeId] = {
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      block.generationSignature ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntBytes) { case (bytes, tx) =>
        val txBytes = tx.companion.bytes(tx)
        bytes ++ Ints.toByteArray(txBytes.size) ++ txBytes
      }
    }

  }

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = ???
}
