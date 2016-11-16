package scorex.core.consensus

import scorex.core.NodeViewModifier


trait SyncInfo {
  def answer: Boolean

  def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)]
}


