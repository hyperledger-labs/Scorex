package examples.trimchain.modifiers

import examples.trimchain.utxo.PersistentAuthenticatedUtxo
import scorex.core.serialization.Serializer
import scorex.core.ModifierTypeId
import scorex.util.ModifierId

class UtxoSnapshot(override val parentId: ModifierId,
                   header: BlockHeader,
                   utxo: PersistentAuthenticatedUtxo) extends TModifier {

  override val modifierTypeId: ModifierTypeId = TModifier.UtxoSnapshot

  //todo: check statically or dynamically output size
  override def id: ModifierId = header.id

  //todo: for Dmitry: implement header + utxo root printing

  override type M = UtxoSnapshot

  //todo: for Dmitry: implement: dump all the boxes
  override def serializer: Serializer[M] = ???
}
