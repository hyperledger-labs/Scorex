package scorex.core.transaction

import scorex.core.NodeViewComponent

import scala.util.Try

/**
  * Unconfirmed transactions pool
 *
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: Transaction[_], M <: MemoryPool[TX, M]] extends NodeViewComponent {
  import scorex.core.NodeViewModifier.ModifierId

  //getters
  def getById(id: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  //modifiers
  def put(tx: TX): Try[M]

  def put(txs: Iterable[TX]): Try[M]

  def putWithoutCheck(txs: Iterable[TX]): M

  def remove(tx: TX): M

  def take(limit: Int): Iterable[TX]

  def filter(id: Array[Byte]): M

  def filter(tx: TX): M

  def filter(txs: Seq[TX]): M
}