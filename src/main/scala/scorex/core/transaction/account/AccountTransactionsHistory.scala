package scorex.core.transaction.account

import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction[P, TX]] {
  def accountTransactions(id: P): Array[TX]
}
