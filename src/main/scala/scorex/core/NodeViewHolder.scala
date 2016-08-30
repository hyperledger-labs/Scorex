package scorex.core

import diode.Circuit
import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.transaction.{MemoryPool, NodeStateModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Wallet

trait NodeViewComponent{
  self =>

  type NS >: self.type <: NodeViewComponent

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

  val circuit = new Circuit[NodeState] {

    override lazy val initialModel = genesisState

    override val actionHandler: HandlerFunction =
      (model, action) => action match {
        case _ => None
      }
  }

  protected def componentsSeq: Seq[NodeViewComponent] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  )

  def apis: Seq[ApiRoute] = componentsSeq.map(_.companion.api)

  import shapeless.syntax.typeable._

  def synchronizeables: Seq[Synchronizable] = componentsSeq.flatMap(_.cast[Synchronizable])
}