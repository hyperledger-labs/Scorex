package scorex.testkit.generators

import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}

/**
  * Produces a modifier with transactions, not necessary syntatically or semantically valid
  */
trait ArbitraryTransactionsCarryingModifierProducer[TX <: Transaction,
    PM <: PersistentNodeViewModifier,
    CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX]] {

  def modifierWithTransactions(memoryPoolOpt: Option[MempoolReader[TX]], customTransactionsOpt: Option[Seq[TX]]): CTM
}
