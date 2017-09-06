package scorex.core.transaction.state

import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{NodeViewComponent, PersistentNodeViewModifier, VersionTag}

import scala.util.Try

trait StateFeature

trait TransactionValidation[P <: Proposition, TX <: Transaction[P]] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}

trait BalanceSheet[P <: Proposition] extends StateFeature {
  def balance(id: P, height: Option[Int] = None): Long
}

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction[P]] extends StateFeature {
  def accountTransactions(id: P): Array[TX]
}


/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]] extends NodeViewComponent {
  self: MS =>

  //must be ID of last applied modifier
  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]
}
