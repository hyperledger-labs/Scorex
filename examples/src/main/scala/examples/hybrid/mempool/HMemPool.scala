package examples.hybrid.mempool

import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.util.{Success, Try}


case class HMemPool(unconfirmed: Map[NodeViewModifier.ModifierId, SimpleBoxTransaction])
  extends MemoryPool[SimpleBoxTransaction, HMemPool] with ScorexLogging {
  override type NVCT = HMemPool

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(id)

  //get mempool transaction ids not presenting in ids
  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = {
    val idsM = ids.map(id => new mutable.WrappedArray.ofByte(id))
    unconfirmed.filter { case (id, tx) =>
      !idsM.contains(new mutable.WrappedArray.ofByte(id))
    }.keySet.map(_.toArray).toSeq
  }

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] =
    ids.flatMap(getById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[HMemPool] =
  Success(HMemPool(unconfirmed + (tx.id -> tx)))

  override def put(txs: Iterable[SimpleBoxTransaction]): Try[HMemPool] =
    Success(HMemPool(unconfirmed ++ txs.map(tx => tx.id -> tx)))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): HMemPool =
    HMemPool(unconfirmed ++ txs.map(tx => tx.id -> tx))

  override def remove(tx: SimpleBoxTransaction): HMemPool =
    HMemPool(unconfirmed - tx.id)

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(id: Array[Byte]): HMemPool = HMemPool(unconfirmed - id)

  override def filter(tx: SimpleBoxTransaction): HMemPool = filter(tx.id)

  override def filter(txs: Seq[SimpleBoxTransaction]): HMemPool = {
    val idsM = txs.map(tx => new mutable.WrappedArray.ofByte(tx.id))

    val newU = unconfirmed.filter { case (id, tx) =>
      !idsM.contains(new mutable.WrappedArray.ofByte(id))
    }
    HMemPool(newU)
  }

}


object HMemPool {
  lazy val emptyPool: HMemPool = HMemPool(Map())
}