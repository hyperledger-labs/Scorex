package scorex.transaction

/**
  * Unconfirmed transactions pool
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: Transaction[_, TX]] {

  def put(tx: TX): MemoryPool[TX]

  def put(txs: Traversable[TX]): MemoryPool[TX]

  //def all(): Seq[TX]

  def getById(id: Array[Byte]): Option[TX]

  def remove(tx: TX)

  def drain(): (Traversable[TX], MemoryPool[TX])

  def filter(id: Array[Byte]): MemoryPool[TX]

  def filter(tx: TX): MemoryPool[TX]

  def filter(txs: Traversable[TX]): MemoryPool[TX]

  //def onNewOffchainTransaction(transaction: TX): Unit
}