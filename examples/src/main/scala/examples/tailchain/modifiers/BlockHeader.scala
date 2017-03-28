package examples.tailchain.modifiers
import examples.hybrid.state.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer


class BlockHeader extends TModifier {
  override def parentId: ModifierId = ???

  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = TModifier.Header

  //todo: check statically or dynamically output size
  override def id: ModifierId = ???

  override def json: Json = ???

  override type M = this.type

  override def serializer: Serializer[BlockHeader.this.type] = ???
}
