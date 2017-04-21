package examples.trimchain.modifiers

import examples.commons.SimpleBoxTransaction
import examples.trimchain.utxo.PersistentAuthenticatedUtxo
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class UtxoSnapshot(override val parentId: ModifierId,
                   header: BlockHeader,
                   utxo: PersistentAuthenticatedUtxo) extends TModifier {
  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = TModifier.UtxoSnapshot

  //todo: check statically or dynamically output size
  override def id: ModifierId = header.id

  //todo: for Dmitry: implement header + utxo root printing
  override def json: Json = ???

  override type M = this.type

  //todo: for Dmitry: implement: dump all the boxes
  override def serializer: Serializer[UtxoSnapshot.this.type] = ???
}
