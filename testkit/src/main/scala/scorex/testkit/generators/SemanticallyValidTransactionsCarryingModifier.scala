package scorex.testkit.generators

import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState


trait SemanticallyValidTransactionsCarryingModifier[TX <: Transaction,
                                                    PM <: PersistentNodeViewModifier,
                                                    CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX],
                                                    ST <: MinimalState[PM, ST]] {

  def semanticallyValidModifier(state: ST): CTM
  def genValidTransactionPair(state: ST): Seq[TX]
  def semanticallyValidModifierWithCustomTransactions(state: ST, transactions: Seq[TX]): CTM
}
