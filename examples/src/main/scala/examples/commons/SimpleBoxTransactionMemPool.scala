package examples.commons

import akka.actor.{ActorRef, ActorSystem}
import scorex.core.transaction.{ReferenceMempool, ReferenceMempoolActor}
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class SimpleBoxTransactionMemPool(unconfirmed: TrieMap[ModifierId, SimpleBoxTransaction])
  extends ReferenceMempool[SimpleBoxTransaction, SimpleBoxTransactionMemPool] {

  //getters
  override def modifierById(id: ModifierId): Option[SimpleBoxTransaction] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[SimpleBoxTransaction] = ids.flatMap(modifierById)

  //modifiers
  override def put(tx: SimpleBoxTransaction): Try[SimpleBoxTransactionMemPool] = Success {
    unconfirmed.put(tx.id, tx)
    this
  }

  override def putWithoutCheck(tx: SimpleBoxTransaction): SimpleBoxTransactionMemPool = {
    unconfirmed.put(tx.id, tx)
    this
  }

  override def remove(tx: SimpleBoxTransaction): SimpleBoxTransactionMemPool = {
    unconfirmed.remove(tx.id)
    this
  }

  override def take(limit: Int): Iterable[SimpleBoxTransaction] =
    unconfirmed.values.toSeq.sortBy(_.fee)(Ordering[Long].reverse).take(limit)

  override def removeBy(condition: SimpleBoxTransaction => Boolean): SimpleBoxTransactionMemPool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}

object SimpleBoxTransactionMemPool {
  def emptyPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool(TrieMap())

  def createMempoolActor(implicit system: ActorSystem): ActorRef = {
    ReferenceMempoolActor[SimpleBoxTransaction, SimpleBoxTransactionMemPool](emptyPool)
  }
}
