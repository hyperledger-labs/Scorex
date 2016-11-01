package examples.curvepos.transaction

import scorex.core.NodeViewComponentCompanion
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

  override def filter(id: Array[Byte]): SimpleMemPool = {
    unconfTxs.remove(key(id))
    this
  }

  override def filter(tx: SimpleTransaction): SimpleMemPool = filter(Seq(tx))

  override def filter(txs: Seq[SimpleTransaction]): SimpleMemPool = {
    txs.foreach(tx => unconfTxs.remove(key(tx.id)))
    this
  }

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

  //get mempool transaction ids not presenting in ids
  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = {
    val idsM = ids.map(id => new mutable.WrappedArray.ofByte(id))
    unconfTxs.filter { case (id, tx) =>
      !idsM.contains(id)
    }.keySet.map(_.toArray).toSeq
  }

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleTransaction] = unconfTxs.values.toSeq

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = SimpleMemPool
}
