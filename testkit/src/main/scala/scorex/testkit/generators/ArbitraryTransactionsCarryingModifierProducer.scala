package scorex.testkit.generators

import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.box.proposition.Proposition

/**
  * Produces a modifier with transactions, not necessary syntatically or semantically valid
   */
trait ArbitraryTransactionsCarryingModifierProducer[
TX <: Transaction,
MPool <: MemoryPool[TX, MPool],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX]] {

def modifierWithTransactions(memoryPoolOpt: Option[MPool], customTransactionsOpt: Option[Seq[TX]]): CTM
}
