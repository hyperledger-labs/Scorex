package scorex.core

import scorex.core.NodeViewHolder.NodeViewHolderEvent
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.{HistoryReader, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.transaction.state.StateReader

// Messages shared by NodeViewHolder, NodeViewSynchronizer and LocalInterface
object NodeViewLocalInterfaceSharedMessages {
  object ReceivableMessages {

    // As NodeViewChange is sealed, the entire family had to come here
    sealed trait NodeViewChange extends NodeViewHolderEvent
    // Received by LocalInterface
    case class ChangedState[SR <: StateReader, PMOD <: PersistentNodeViewModifier](reader: SR, progressInfoOpt: Option[ProgressInfo[PMOD]])
      extends NodeViewChange
    // Received by LocalInterface
    case class ChangedStateFailed[SR <: StateReader, PMOD <: PersistentNodeViewModifier](reader: SR, progressInfoOpt: Option[ProgressInfo[PMOD]])
      extends NodeViewChange
    // Received by NodeViewSynchcronizer
    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange
    //TODO: return mempool reader
    // Received by NodeViewSynchcronizer
    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction[_]]](mempool: MR) extends NodeViewChange
    //TODO: return Vault reader
    // No actor process this message explicitly, it seems to be sent to NodeViewSynchcronizer
    case class ChangedVault() extends NodeViewChange

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

