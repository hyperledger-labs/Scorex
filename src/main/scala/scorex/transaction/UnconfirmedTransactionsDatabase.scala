package scorex.transaction

import scorex.block.TransactionalData

trait UnconfirmedTransactionsDatabase[TX <: Transaction[_, TX], TData <: TransactionalData[TX]] {

  def putIfNew(tx: TX): Boolean

  def all(): Seq[TX]

  def getById(id: Array[Byte]): Option[TX]

  def packUnconfirmed(): TData

  def clearFromUnconfirmed(data: TData): Unit

  def onNewOffchainTransaction(transaction: TX): Unit

  def remove(tx: TX)
}