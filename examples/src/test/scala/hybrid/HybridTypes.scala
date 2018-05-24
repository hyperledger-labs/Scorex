package hybrid

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.state.HBoxStoredState
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridTypes {

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo
  type HSI = HybridSyncInfo
  type SIS = HybridSyncInfoMessageSpec.type

  type NODE = HybridNodeViewHolder
  type ST = HBoxStoredState
  type HT = HybridHistory
  type MP = SimpleBoxTransactionMemPool

}