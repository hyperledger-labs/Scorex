package scorex.core

import akka.actor.Actor
import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.NodeViewSynchronizer.{GetContinuation, PartialOpenSurface}
import scorex.core.transaction.NodeStateModifier.ModifierId
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

  def produceModification[M <: NodeStateModifier, CompType <: NodeViewComponent](component: CompType, m: M): UndoneModification[M, CompType]

  //network functions to call
}

trait Synchronizable {
  val invId: Byte
}

sealed trait Modification[M <: NodeStateModifier, VC <: NodeViewComponent] {
  val reason: M
  val component: VC
}

trait UndoneModification[M <: NodeStateModifier, VC <: NodeViewComponent]
  extends Modification[M, VC] {

  def process(): DoneModification[M, VC]
}

trait DoneModification[M <: NodeStateModifier, VC <: NodeViewComponent]
  extends Modification[M, VC] {

  def flatMap[VC2 <: NodeViewComponent](component: VC2): DoneModification[M, VC2] = {
    this match {
      case sm: SuccessfulModification[M, VC] =>
        val modification = component.companion.produceModification(component, sm.reason)
        modification.process()
      case FailedModification(_, r: M, e) =>
        FailedModification(component, r, e)
    }
  }
}


trait SuccessfulModification[M <: NodeStateModifier, VC <: NodeViewComponent]
  extends Modification[M, VC] {

  val result: VC
}

trait SuccessWithRebranch[M <: NodeStateModifier, VC <: NodeViewComponent]
  extends SuccessfulModification[M, VC] {

  val rollbackTo: ModifierId
  val result: VC
}

trait Error {
  val message: String
}

case class FailedModification[M <: NodeStateModifier, VC <: NodeViewComponent]
(override val component: VC,
 override val reason: M,
 error: Error) extends DoneModification[M, VC]


//todo: listeners
//todo: async update?

/**
  * Composite local view
  *
  * @tparam P
  * @tparam TX
  */
/*
 (S, H, MP, W)
 - process state modifier M in following order (H -> S -> MP -> W)

 - HxM -> Outcome[H]
 */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P, TX]] extends Actor {

  type HIS <: History[P, TX, _]
  type MS <: MinimalState[P, TX]
  type WL <: Wallet[P, TX]
  type MP <: MemoryPool[TX]

  type NodeState = (HIS, MS, WL, MP)

  def restoreState(): Option[NodeState]

  private var nodeState: NodeState = restoreState().getOrElse(genesisState)

  def history(): HIS = nodeState._1

  def minimalState(): MS = nodeState._2

  def wallet(): WL = nodeState._3

  def memoryPool(): MP = nodeState._4

  lazy val historyCompanion = history().companion

  def modify[MOD <: NodeStateModifier](m: MOD) = {
    val modification = historyCompanion.produceModification(history(), m)
    val hd = modification.process()
    val md = hd.flatMap[MS](minimalState())
    val wld = md.flatMap[WL](wallet())
    val mpd = wld.flatMap[MP](memoryPool())
  }

  protected def genesisState: NodeState

  def apis: Seq[ApiRoute] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  ).map(_.companion.api)

  override def receive: Receive = {
    case PartialOpenSurface(sid, modifierTypeId, modifierIds) =>
      modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          sender() ! NodeViewSynchronizer.RequestFromLocal(sid, typeId, memoryPool().notIn(modifierIds)
      }

    case GetContinuation(sid, modifierTypeId, modifierIds) =>
      modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          sender() ! NodeViewSynchronizer.RequestFromLocal(sid, typeId, memoryPool().notIn(modifierIds)
      }

  }
}