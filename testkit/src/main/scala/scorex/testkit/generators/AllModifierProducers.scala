package scorex.testkit.generators

import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.Transaction
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}


trait AllModifierProducers[TX <: Transaction,
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX],
ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]]
  extends SemanticallyValidModifierProducer[PM, ST]
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with ArbitraryTransactionsCarryingModifierProducer[TX, PM, CTM]
    with TotallyValidModifierProducer[PM, ST, SI, HT]
    with SemanticallyValidTransactionsCarryingModifier[TX, PM, CTM, ST]
