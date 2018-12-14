package scorex.core.transaction

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.NodeViewComponent.MempoolComponent
import scorex.core.NodeViewComponentOperation.GetReader
import scorex.core.transaction.MempoolOperation._

import scala.util.Try

/** Trivial mempool implementation just for scratching purposes, not for real nodes. Bases on `ReferenceMempool`
  */
class ReferenceMempoolActor[TX <: Transaction, M <: ReferenceMempool[TX, M]](private var mempool: M) extends Actor {

  def receive: Receive = put orElse remove orElse getReader

  protected def put: Receive = {
    case Put(tx, mode) if mode == PutWithoutCheck =>
      mempool = mempool.putWithoutCheck(tx.asInstanceOf[TX])

    case Put(tx, mode) =>
      val result = mempool.put(tx.asInstanceOf[TX])
      result.foreach(mempool = _)
      sender ! PutResponse(tx, result.map(_ => ()), mode)
  }

  protected def remove: Receive = {
    case Remove(tx) =>
      mempool = mempool.remove(tx.asInstanceOf[TX])

    case RemoveBy(condition) =>
      mempool = mempool.removeBy(condition.asInstanceOf[TX => Boolean])
  }

  protected def getReader: Receive = {
    case GetReader(MempoolComponent) =>
      val reader = mempool.getReader
      sender ! reader
  }
}

object ReferenceMempoolActor {

  def props[TX <: Transaction, M <: ReferenceMempool[TX, M]](mempool: M): Props = {
    Props(new ReferenceMempoolActor[TX, M](mempool))
  }

  def apply[TX <: Transaction, M <: ReferenceMempool[TX, M]](mempool: M)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(props[TX, M](mempool))
  }
}

/** Trivial mempool interface, mostly for backward compatibility with old `MemoryPool` trait
  * @tparam TX - type of transactions that the pool contains
  */
trait ReferenceMempool[TX <: Transaction, M <: ReferenceMempool[TX, M]] extends MempoolReader[TX] {

  /** Method to put a transaction into the memory pool. Validation of tha transactions against
    * the state is done in NodeVieHolder. This put() method can check whether a transaction is valid
    * @return Success(updatedPool), if transaction successfully added to the pool, or Failure otherwise
    */
  def put(tx: TX): Try[M]

  def putWithoutCheck(tx: TX): M

  def remove(tx: TX): M

  def removeBy(condition: TX => Boolean): M

  /** Get the reader for the memory pool
    */
  def getReader: MempoolReader[TX] = this
}
