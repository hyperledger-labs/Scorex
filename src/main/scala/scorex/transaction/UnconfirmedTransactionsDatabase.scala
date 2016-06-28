package scorex.transaction

trait UnconfirmedTransactionsDatabase[TX <: Transaction[_, TX]] {

  def putIfNew(tx: TX): Boolean

  def all(): Seq[TX]

  def getBySignature(signature: Array[Byte]): Option[TX]

  def remove(tx: TX)
}