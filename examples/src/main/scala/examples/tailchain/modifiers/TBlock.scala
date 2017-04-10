package examples.tailchain.modifiers

import examples.commons.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
import scorex.core.block.Block.{Timestamp, Version}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class TBlock(header: BlockHeader, body: Seq[SimpleBoxTransaction])
  extends TModifier with Block[PublicKey25519Proposition, SimpleBoxTransaction] {

  override def version: Version = 0: Version

  override def timestamp: Timestamp = ???

  //todo: for Dmitry: implement
  override def json: Json = ???

  override def parentId: ModifierId = ???

  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = Some(body)

  override val modifierTypeId: ModifierTypeId = TModifier.Block

  //todo: check statically or dynamically output size
  override def id: ModifierId = header.id

  override type M = this.type

  //todo: for Dmitry: implement
  override def serializer: Serializer[TBlock.this.type] = ???
}