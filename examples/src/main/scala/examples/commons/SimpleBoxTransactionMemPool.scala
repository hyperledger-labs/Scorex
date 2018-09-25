package examples.commons

import scorex.core.transaction.MemoryPool
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxTransactionMemPool(unconfirmed: TrieMap[ModifierId, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, SimpleBoxTransactionMemPool] {
  override type NVCT = SimpleBoxTransactionMemPool

  //getters
  override def modifierById(id: ModifierId): Option[SimpleBoxTransaction] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[SimpleBoxTransactionMemPool] = Success {
    unconfirmed.put(tx.id, tx)
    this
  }

  //todo
  override def put(txs: Iterable[SimpleBoxTransaction]): Try[SimpleBoxTransactionMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): SimpleBoxTransactionMemPool = {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    this
  }

  override def remove(tx: SimpleBoxTransaction): SimpleBoxTransactionMemPool = {
    unconfirmed.remove(tx.id)
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: SimpleBoxTransaction => Boolean): SimpleBoxTransactionMemPool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object SimpleBoxTransactionMemPool {
  lazy val emptyPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool(TrieMap())
}
