package examples.curvepos.transaction

import io.circe.Json
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.NodeViewModifierCompanion
import shapeless.{HNil, ::}

import scala.util.Try

import Block._

case class SimpleBlock(txs: Seq[SimpleTransaction],
                       override val parentId: BlockId,
                       override val timestamp: Long,
                       generator: PublicKey25519Proposition)
  extends Block[PublicKey25519Proposition, SimpleTransaction] {

  override def transactions: Option[Seq[SimpleTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[SimpleBlock] = SimpleBlockCompanion

  override def id: BlockId =
    FastCryptographicHash(txs.map(_.messageToSign).reduce(_ ++ _) ++ generator.bytes)

  override type B = SimpleBlock

  override type M = SimpleBlock

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long

  override type BlockFields = Seq[SimpleTransaction] :: Timestamp :: Version :: HNil

  override val version: Version = 0: Byte

  override def bytes: Array[Byte] = ???

  override def json: Json = ???

  override lazy val blockFields: BlockFields = txs :: timestamp :: version :: HNil
}

object SimpleBlockCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  override def bytes(modifier: SimpleBlock): Array[ModifierTypeId] = ???

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = ???
}
