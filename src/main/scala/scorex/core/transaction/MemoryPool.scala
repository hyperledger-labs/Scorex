package scorex.core.transaction

import scorex.core.NodeViewComponent

import scala.util.Try

/**
  * Unconfirmed transactions pool
 *
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: Transaction[_, TX], M <: MemoryPool[TX, M]] extends NodeViewComponent {
  import NodeViewModifier.ModifierId

  //getters
  def getById(id: ModifierId): Option[TX]

  def notIn(ids: Seq[ModifierId]): Seq[ModifierId]

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  //modifiers
  def put(tx: TX): Try[M]

  def put(txs: Iterable[TX]): Try[M]

  def putWithoutCheck(txs: Iterable[TX]): M

  def remove(tx: TX): M

  /**
   * Get sequence of transactions and remove them from pool
   */
  def drain(limit: Int): (Iterable[TX], M)

  def filter(id: Array[Byte]): M

  def filter(tx: TX): M

  def filter(txs: Iterable[TX]): M
}