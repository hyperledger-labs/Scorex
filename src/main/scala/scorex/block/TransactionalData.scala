package scorex.block

import scorex.transaction.Transaction

trait TransactionalData[TX <: Transaction[_, TX]] {
  val mbTransactions: Option[Traversable[TX]]

  val headerOnly = mbTransactions.isDefined
}

