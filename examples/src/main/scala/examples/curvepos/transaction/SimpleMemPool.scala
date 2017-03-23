package examples.curvepos.transaction

import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try

class SimpleMemPool extends MemoryPool[SimpleTransaction, SimpleMemPool] {

  private val unconfTxs: TrieMap[TxKey, SimpleTransaction] = TrieMap()
  private type TxKey = scala.collection.mutable.WrappedArray.ofByte

  private def key(id: Array[Byte]): TxKey = new mutable.WrappedArray.ofByte(id)

  //getters
  override def getById(id: ModifierId): Option[SimpleTransaction] = unconfTxs.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfTxs.contains(key(id))

  override def putWithoutCheck(txs: Iterable[SimpleTransaction]): SimpleMemPool = {
    txs.foreach(tx => unconfTxs.put(key(tx.id), tx))
    this
  }

  //modifiers
  override def put(tx: SimpleTransaction): Try[SimpleMemPool] = put(Seq(tx))

  override def put(txs: Iterable[SimpleTransaction]): Try[SimpleMemPool] = Try {
    txs.foreach(tx => require(!unconfTxs.contains(key(tx.id))))
    putWithoutCheck(txs)
  }

  override def take(limit: Int): Iterable[SimpleTransaction] =
    unconfTxs.keys.take(limit).flatMap(k => unconfTxs.get(k))

  override def remove(tx: SimpleTransaction): SimpleMemPool = filter(tx)

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleTransaction] = unconfTxs.values.toSeq

  override type NVCT = SimpleMemPool

  override def filter(condition: (SimpleTransaction) => Boolean): SimpleMemPool = {
    unconfTxs.filter(tx => condition(tx._2)).foreach(tx => unconfTxs.remove(tx._1))
    this
  }
}
