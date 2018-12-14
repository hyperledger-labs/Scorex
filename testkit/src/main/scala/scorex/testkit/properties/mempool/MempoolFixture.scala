package scorex.testkit.properties.mempool

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import org.scalacheck.Gen
import scorex.core.NodeViewComponent.MempoolComponent
import scorex.core.NodeViewComponentOperation.GetReader
import scorex.core.transaction.MempoolOperation._
import scorex.core.transaction.{MempoolReader, ReferenceMempool, Transaction}
import scorex.testkit.utils.BaseActorFixture
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class MempoolFixture[TX <: Transaction](mempoolActorGen: ActorSystem => ActorRef)
  extends BaseActorFixture with ReferenceMempool[TX, MempoolFixture[TX]] {

  type M = MempoolFixture[TX]

  val mempoolActor: ActorRef = mempoolActorGen(system)

  /** Mempool generator that clean mempool on every generation and reuse it
    */
  val mempoolGenerator: Gen[M] = Gen.const(this).map({r => clear(); r})

  implicit protected val timeout: Timeout = Timeout(10.seconds)

  def await[T](future: Future[T]): T = Await.result(future, timeout.duration)

  def put(tx: TX): Try[M] = {
    await((mempoolActor ? Put(tx)).mapTo[PutResponse[TX]]).result.map(_ => this)
  }

  def putWithoutCheck(tx: TX): M = {
    mempoolActor ! PutWithoutCheck(tx)
    this
  }

  def remove(tx: TX): M = {
    mempoolActor ! Remove(tx)
    this
  }

  def removeBy(condition: TX => Boolean): M = {
    mempoolActor ! RemoveBy(condition)
    this
  }

  def clear(): Unit = {
    mempoolActor ! RemoveBy({ _: TX => true })
  }

  /** Get the reader for the memory pool
    */
  override def getReader: MempoolReader[TX] = {
    await((mempoolActor ? GetReader(MempoolComponent)).mapTo[MempoolReader[TX]])
  }

  override def modifierById(modifierId: ModifierId): Option[TX] = {
    getReader.modifierById(modifierId)
  }

  override def contains(id: ModifierId): Boolean = {
    getReader.contains(id)
  }

  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = {
    val reader = getReader
    ids.filter(id => !reader.contains(id))
  }

  def getAll(ids: Seq[ModifierId]): Seq[TX] = {
    getReader.getAll(ids)
  }

  def size: Int = {
    getReader.size
  }

  def take(limit: Int): Iterable[TX] = {
    getReader.take(limit)
  }

}
