package scorex.transaction.account

import scorex.transaction.box.proposition.Proposition


trait BalanceSheet[P <: Proposition] {

  def balance(id: P, height: Option[Int] = None): Long

}
