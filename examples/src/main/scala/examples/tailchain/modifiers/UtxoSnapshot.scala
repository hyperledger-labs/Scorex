package examples.tailchain.modifiers

import examples.commons.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class UtxoSnapshot extends TModifier {
  override def parentId: ModifierId = ???

  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = TModifier.UtxoSnapshot

  //todo: check statically or dynamically output size
  override def id: ModifierId = ???

  //todo: for Dmitry: implement
  override def json: Json = ???

  override type M = this.type

  //todo: for Dmitry: implement
  override def serializer: Serializer[UtxoSnapshot.this.type] = ???
}
