package scorex.testkit.generators

import scorex.core.consensus.{History, SyncInfo}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.{MemoryPool, Transaction}

trait SemanticallyValidModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] {
  def semanticallyValidModifier(state: ST): PM
}

trait SemanticallyValidTransactionsCarryingModifier[P <: Proposition, TX <: Transaction[P],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[P, TX],
ST <: MinimalState[PM, ST]] {

  def semanticallyValidModifierWithTransactions(state: ST): CTM
  def genValidTransactionPair(state: ST): Seq[TX]
  def semanticallyValidModifierWithCustomTransactions(state: ST, transactions: Seq[TX]): CTM
}

trait SyntaticallyValidModifierProducer[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] {
  def syntaticallyValidModifier(history: HT): PM
}

  /**
    * Produces a modifier with transactions, not necessary syntatically or semantically valid
     */
  trait ArbitraryTransactionsCarryingModifierProducer[P <: Proposition,
  TX <: Transaction[P],
  MPool <: MemoryPool[TX, MPool],
  PM <: PersistentNodeViewModifier,
  CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[P, TX]] {

  def modifierWithTransactions(memoryPoolOpt: Option[MPool], customTransactionsOpt: Option[Seq[TX]]): CTM
}

trait TotallyValidModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]] {

  def totallyValidModifier(history: HT, state: ST): PM
}


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