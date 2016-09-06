package scorex.core.transaction.account

import scorex.core.transaction.box.proposition.Proposition


trait BalanceSheet[P <: Proposition] {

  def balance(id: P, height: Option[Int] = None): Long

}
