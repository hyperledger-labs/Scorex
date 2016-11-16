package examples.curvepos

import examples.curvepos.transaction.SimpleBlock
import scorex.core.NodeViewModifier
import scorex.core.consensus.{BlockChain, SyncInfo}

case class SimpleSyncInfo(answer: Boolean, lastBlockID: NodeViewModifier.ModifierId, score: BlockChain.Score) extends SyncInfo {
  override def startingPoints: Seq[(NodeViewModifier.ModifierTypeId, NodeViewModifier.ModifierId)] = {
    Seq(SimpleBlock.ModifierTypeId -> lastBlockID)
  }
}