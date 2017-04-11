package examples.hybrid.mempool

import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class HMemPool(unconfirmed: TrieMap[ByteArrayWrapper, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, HMemPool] with ScorexLogging {
  override type NVCT = HMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[HMemPool] = Success {
    unconfirmed.put(key(tx.id), tx)
    this
  }

  //todo
  override def put(txs: Iterable[SimpleBoxTransaction]): Try[HMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): HMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    this
  }

  override def remove(tx: SimpleBoxTransaction): HMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (SimpleBoxTransaction) => Boolean): HMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object HMemPool {
  lazy val emptyPool: HMemPool = HMemPool(TrieMap())
}