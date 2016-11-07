package scorex.core.consensus

import scorex.core.NodeViewModifier
import scorex.core.serialization.BytesSerializable


trait SyncInfo extends BytesSerializable {
  def answer: Boolean
  def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)]
}


