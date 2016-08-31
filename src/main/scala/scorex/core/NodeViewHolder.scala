package scorex.core

import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.transaction.{MemoryPool, NodeStateModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Wallet


trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent

  def companion: NodeViewComponentCompanion
}

trait NodeViewComponentCompanion {
  def api: ApiRoute

  //network functions to call
}

trait Synchronizable {
  val invId: Byte
}

//todo: listeners
//todo: async update?
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P, TX], M <: NodeStateModifier] {
  self =>

  type NVH >: self.type <: NodeViewHolder[P, TX, M]

  type MS <: MinimalState[P, TX]
  type HIS <: History[P, TX]
  type MP <: MemoryPool[TX]
  type WL <: Wallet[P, TX]

  type NodeState = (MS, HIS, MP, WL)

  protected def genesisState: NodeState


  def apis: Seq[ApiRoute] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  ).map(_.companion.api)
}