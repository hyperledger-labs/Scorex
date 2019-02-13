package scorex.core.transaction.state

import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait MinimalState[M <: PersistentNodeViewModifier, State <: MinimalState[M, State]] extends StateReader {
  self: State =>

  def applyModifier(mod: M): Try[State]

  def rollbackTo(version: VersionTag): Try[State]

  /** read-only copy of this state
    */
  def getReader: StateReader = this

}


trait StateFeature

trait TransactionValidation[TX <: Transaction] extends StateFeature {
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

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction] extends StateFeature {
  def accountTransactions(id: P): Array[TX]
}
