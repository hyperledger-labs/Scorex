package scorex.transaction.account

import scorex.transaction.Transaction
import scorex.transaction.box.Proposition

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction[P, TX]] {
  def accountTransactions(id: P): Array[TX]
}