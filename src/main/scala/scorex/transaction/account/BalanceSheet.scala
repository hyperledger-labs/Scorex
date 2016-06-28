package scorex.transaction.account

import scorex.transaction.box.Proposition


trait BalanceSheet[P <: Proposition] {
  val GenerationBalanceConfirmations = 50

  def balance(id: P, height: Option[Int] = None): Long

  def balanceWithConfirmations(id: P, confirmations: Int): Long

  def generationBalance(id: P): Long = balanceWithConfirmations(id, GenerationBalanceConfirmations)
}
