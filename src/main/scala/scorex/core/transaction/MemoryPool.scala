package scorex.core.transaction

import scorex.core.NodeViewComponent

/**
  * Unconfirmed transactions pool
 *
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: Transaction[_, TX]] extends NodeViewComponent {
  import NodeStateModifier.ModifierId

  //getters
  def getById(id: ModifierId): Option[TX]

  def notIn(ids: Seq[ModifierId]): Seq[ModifierId]

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  //modifiers
  def put(tx: TX): MemoryPool[TX]

  def put(txs: Traversable[TX]): MemoryPool[TX]

  def remove(tx: TX): MemoryPool[TX]

  /**
   * Get sequence of transactions and remove them from pool
   */
  def drain(limit: Int): (Traversable[TX], MemoryPool[TX])

  def take(limit: Int): (Traversable[TX], MemoryPool[TX])

  def filter(id: Array[Byte]): MemoryPool[TX]

  def filter(tx: TX): MemoryPool[TX]

  def filter(txs: Traversable[TX]): MemoryPool[TX]
}