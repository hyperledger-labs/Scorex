package scorex.core

import scorex.core.transaction.NodeStateModifier
import scorex.core.transaction.NodeStateModifier._


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

  def join[VC2 <: NodeViewComponent](component: VC2): DoneModification[M, VC2] = {
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
  extends DoneModification[M, VC] {

  val result: VC
}

trait SuccessWithRebranch[M <: NodeStateModifier, VC <: NodeViewComponent]
  extends SuccessfulModification[M, VC] {

  val rollbackTo: ModifierId
}

trait Error {
  val message: String
}

case class FailedModification[M <: NodeStateModifier, VC <: NodeViewComponent]
(override val component: VC,
 override val reason: M,
 error: Error) extends DoneModification[M, VC]