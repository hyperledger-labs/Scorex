package scorex.block

import scorex.transaction.Transaction
import shapeless.{HNil, HList}


trait TransactionalData[TX <: Transaction[_, TX]] {
  type TransactionalHeaderFields <: HList

  val mbTransactions: Option[Traversable[TX]]

  val transactionalHeaderFields: TransactionalHeaderFields

  val headerOnly = mbTransactions.isDefined

  val transactionalFields = mbTransactions match {
    case Some(txs) => transactionalHeaderFields :: txs :: HNil
    case None => transactionalHeaderFields
  }
}

