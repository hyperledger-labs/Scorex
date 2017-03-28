package examples.tailchain.modifiers
import examples.hybrid.state.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer


class UtxoSnapshot extends TModifier {
  override def parentId: ModifierId = ???

  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = ???

  override val modifierTypeId: ModifierTypeId = TModifier.UtxoSnapshot

  //todo: check statically or dynamically output size
  override def id: ModifierId = ???

  override def json: Json = ???

  override type M = this.type

  override def serializer: Serializer[UtxoSnapshot.this.type] = ???
}
