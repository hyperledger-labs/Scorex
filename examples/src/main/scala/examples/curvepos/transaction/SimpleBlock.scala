package examples.curvepos.transaction

import io.circe.Json
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PublicKey25519
import scorex.core.transaction.NodeViewModifierCompanion
import shapeless.HNil

import scala.util.Try

case class SimpleBlock(txs: Seq[SimpleTransaction],

                       generator: PublicKey25519)
  extends Block[PublicKey25519Proposition, SimpleTransaction] {

  override def transactions: Option[Seq[SimpleTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[SimpleBlock] = SimpleBlockCompanion

  override def id: ModifierId =
    FastCryptographicHash(txs.map(_.messageToSign).reduce(_ ++ _) ++ generator.bytes)

  override type B = SimpleBlock

  override type M = SimpleBlock

  override type BlockFields = HNil

  override def version: ModifierTypeId = 0: Byte

  override def parentId: ModifierId = ???

  override def bytes: Array[ModifierTypeId] = ???

  override def json: Json = ???

  override def timestamp: Long = ???
}

object SimpleBlockCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  override def bytes(modifier: SimpleBlock): Array[ModifierTypeId] = ???

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = ???
}
