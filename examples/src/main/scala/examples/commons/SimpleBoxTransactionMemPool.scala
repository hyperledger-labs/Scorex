package examples.commons

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}


case class SimpleBoxTransactionMemPool(unconfirmed: Map[ByteArrayWrapper, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, SimpleBoxTransactionMemPool] with ScorexLogging {
  override type NVCT = SimpleBoxTransactionMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[SimpleBoxTransactionMemPool] =
  Success(SimpleBoxTransactionMemPool(unconfirmed + (key(tx.id) -> tx)))

  override def put(txs: Iterable[SimpleBoxTransaction]): Try[SimpleBoxTransactionMemPool] =
    Success(SimpleBoxTransactionMemPool(unconfirmed ++ txs.map(tx => key(tx.id) -> tx)))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): SimpleBoxTransactionMemPool =
    SimpleBoxTransactionMemPool(unconfirmed ++ txs.map(tx => key(tx.id) -> tx))

  override def remove(tx: SimpleBoxTransaction): SimpleBoxTransactionMemPool =
    SimpleBoxTransactionMemPool(unconfirmed - key(tx.id))

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (SimpleBoxTransaction) => Boolean): SimpleBoxTransactionMemPool = {
    SimpleBoxTransactionMemPool(unconfirmed.filter(tx => condition(tx._2)))
  }
}


object SimpleBoxTransactionMemPool {
  lazy val emptyPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool(Map())
}