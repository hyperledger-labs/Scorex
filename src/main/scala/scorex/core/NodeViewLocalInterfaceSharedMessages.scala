package scorex.core

import scorex.core.NodeViewHolder.NodeViewHolderEvent
import scorex.core.consensus.{HistoryReader, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.transaction.state.StateReader

// Messages shared by NodeViewHolder, NodeViewSynchronizer and LocalInterface
object NodeViewLocalInterfaceSharedMessages {
  object ReceivableMessages {

    //hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent
    case class FailedTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX, error: Throwable) extends ModificationOutcome
    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX) extends ModificationOutcome
    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome
    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome
    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome
    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    //node view holder starting persistent modifier application
    case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent
    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent
    //todo: consider sending info on the rollback
    case object RollbackFailed extends NodeViewHolderEvent
  }
}

