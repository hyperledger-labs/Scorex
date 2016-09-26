package examples.curvepos.transaction

import io.circe.Json
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.NodeViewModifierCompanion
import shapeless.{HNil, ::}

import scala.util.Try

import Block._
import SimpleBlock._

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

  override type B = SimpleBlock

  override type M = SimpleBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[SimpleTransaction] :: HNil

  override val version: Version = 0: Byte

  override def bytes: Array[Byte] = ???

  override def json: Json = ???

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget :: generator :: txs :: HNil
}

object SimpleBlock {
  type GenerationSignature = Array[Byte]

  type BaseTarget = Long
}

object SimpleBlockCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  override def bytes(modifier: SimpleBlock): Array[ModifierTypeId] = ???

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = ???
}
