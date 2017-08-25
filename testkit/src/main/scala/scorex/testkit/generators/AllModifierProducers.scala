package scorex.testkit.generators

import scorex.core.consensus.{History, SyncInfo}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState


trait AllModifierProducers[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[P, TX],
ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]]
  extends SemanticallyValidModifierProducer[PM, ST]
    with SyntaticallyValidModifierProducer[PM, SI, HT]
    with ArbitraryTransactionsCarryingModifierProducer[P, TX, MPool, PM, CTM]
    with TotallyValidModifierProducer[PM, ST, SI, HT]
    with SemanticallyValidTransactionsCarryingModifier[P, TX, PM, CTM, ST]
