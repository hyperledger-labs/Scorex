package hybrid

import examples.commons.SimpleBoxTransaction
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.state.HBoxStoredState
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.network.message.SyncInfoMessageSpec

trait HybridTypes {

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo
  type SIS = SyncInfoMessageSpec[HybridSyncInfo]

  type NODE = HybridNodeViewHolder
  type ST = HBoxStoredState
  type HT = HybridHistory

}