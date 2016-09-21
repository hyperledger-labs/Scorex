package examples.curvepos.transaction

import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.NodeViewModifier.ModifierId

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try

class SimplestMemPool extends MemoryPool[SimplestTransaction, SimplestMemPool] {

  private val pool: TrieMap[TxKey, SimplestTransaction] = TrieMap()
  private type TxKey = scala.collection.mutable.WrappedArray.ofByte

  private def key(id: Array[Byte]): TxKey = new mutable.WrappedArray.ofByte(id)

  //getters
  override def getById(id: ModifierId): Option[SimplestTransaction] = pool.get(key(id))

  override def filter(id: Array[Byte]): SimplestMemPool = {
    pool.remove(key(id))
    this
  }

  override def filter(tx: SimplestTransaction): SimplestMemPool = filter(Seq(tx))

  override def filter(txs: Iterable[SimplestTransaction]): SimplestMemPool = {
    txs.foreach(tx => pool.remove(key(tx.id())))
    this
  }

  override def putWithoutCheck(txs: Iterable[SimplestTransaction]): SimplestMemPool = {
    txs.foreach(tx => pool.put(key(tx.id()), tx))
    this
  }

  //modifiers
  override def put(tx: SimplestTransaction): Try[SimplestMemPool] = put(Seq(tx))

  override def put(txs: Iterable[SimplestTransaction]): Try[SimplestMemPool] = Try {
    txs.foreach(tx => require(!pool.contains(key(tx.id()))))
    putWithoutCheck(txs)
  }

  /**
    * Get sequence of transactions and remove them from pool
    */
  override def drain(limit: Int): (Iterable[SimplestTransaction], SimplestMemPool) = {
    val txs: Iterable[SimplestTransaction] = pool.keys.take(limit).flatMap(k => pool.get(k))
    (txs, filter(txs))
  }

  override def remove(tx: SimplestTransaction): SimplestMemPool = filter(tx)

  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ???

  override def getAll(ids: Seq[ModifierId]): Seq[SimplestTransaction] = {
    pool.keys.flatMap(k => pool.get(k)).toSeq
  }

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = SimplestMemPool
}
