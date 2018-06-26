package scorex.testkit.generators

import scorex.core.consensus.{History, SyncInfo}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.state.MinimalState


trait AllModifierProducers[
TX <: Transaction,
MPool <: MemoryPool[TX, MPool],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX],
ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]]
  extends SemanticallyValidModifierProducer[PM, ST]
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with ArbitraryTransactionsCarryingModifierProducer[TX, MPool, PM, CTM]
    with TotallyValidModifierProducer[PM, ST, SI, HT]
    with SemanticallyValidTransactionsCarryingModifier[TX, PM, CTM, ST]
