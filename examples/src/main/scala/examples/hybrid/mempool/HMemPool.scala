package examples.hybrid.mempool

import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool

import scala.util.{Success, Try}


case class HMemPool(unconfirmed: Map[NodeViewModifier.ModifierId, SimpleBoxTransaction]) extends MemoryPool[SimpleBoxTransaction, HMemPool] {
  override type NVCT = HMemPool

  //getters
  override def getById(id: ModifierId): Option[SimpleBoxTransaction] =
  unconfirmed.get(id)

  //get mempool transaction ids not presenting in ids
  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ???

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ???

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[HMemPool] =
    Success(HMemPool(unconfirmed + (tx.id -> tx)))

  override def put(txs: Iterable[SimpleBoxTransaction]): Try[HMemPool] =
    Success(HMemPool(unconfirmed ++ txs.map(tx => tx.id -> tx)))

  override def putWithoutCheck(txs: Iterable[SimpleBoxTransaction]): HMemPool =
    HMemPool(unconfirmed ++ txs.map(tx => tx.id -> tx))

  override def remove(tx: SimpleBoxTransaction): HMemPool =
    HMemPool(unconfirmed - tx.id)

  override def take(limit: Int): Iterable[SimpleBoxTransaction] = ???

  override def filter(id: Array[Byte]): HMemPool = ???

  override def filter(tx: SimpleBoxTransaction): HMemPool = ???

  override def filter(txs: Iterable[SimpleBoxTransaction]): HMemPool = ???

  override def companion: NodeViewComponentCompanion = ???
}
