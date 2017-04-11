package examples.hybrid.mempool

import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}


case class HMemPool(unconfirmed: Map[ByteArrayWrapper, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, HMemPool] with ScorexLogging {
  override type NVCT = HMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[HMemPool] =
  Success(HMemPool(unconfirmed + (key(tx.id) -> tx)))

  override def put(txs: Iterable[SimpleBoxTransaction]): Try[HMemPool] =
    Success(HMemPool(unconfirmed ++ txs.map(tx => key(tx.id) -> tx)))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): HMemPool =
    HMemPool(unconfirmed ++ txs.map(tx => key(tx.id) -> tx))

  override def remove(tx: SimpleBoxTransaction): HMemPool =
    HMemPool(unconfirmed - key(tx.id))

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (SimpleBoxTransaction) => Boolean): HMemPool = {
    if(unconfirmed.exists(t => condition(t._2))) this
    else HMemPool(unconfirmed.filter(tx => condition(tx._2)))
  }

  override def size: Int = unconfirmed.size
}


object HMemPool {
  lazy val emptyPool: HMemPool = HMemPool(Map())
}