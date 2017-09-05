package examples.commons

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxTransactionMemPool(unconfirmed: TrieMap[ByteArrayWrapper, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, SimpleBoxTransactionMemPool] with ScorexLogging {
  override type NVCT = SimpleBoxTransactionMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[SimpleBoxTransactionMemPool] = Success {
    unconfirmed.put(key(tx.id), tx)
    this
  }

  //todo
  override def put(txs: Iterable[SimpleBoxTransaction]): Try[SimpleBoxTransactionMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): SimpleBoxTransactionMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    this
  }

  override def remove(tx: SimpleBoxTransaction): SimpleBoxTransactionMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (SimpleBoxTransaction) => Boolean): SimpleBoxTransactionMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object SimpleBoxTransactionMemPool {
  lazy val emptyPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool(TrieMap())
}