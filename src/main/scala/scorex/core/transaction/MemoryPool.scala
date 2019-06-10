package scorex.core.transaction

import scala.util.Try

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: Transaction, M <: MemoryPool[TX, M]] extends MempoolReader[TX] {

  /**
    * Method to put a transaction into the memory pool. Validation of tha transactions against
    * the state is done in NodeVieHolder. This put() method can check whether a transaction is valid
    * @param tx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(tx: TX): Try[M]

  def put(txs: Iterable[TX]): Try[M]

  def putWithoutCheck(txs: Iterable[TX]): M

  def remove(tx: TX): M

  def filter(txs: Seq[TX]): M = filter(t => !txs.exists(_.id == t.id))

  def filter(condition: TX => Boolean): M

  /**
    * @return read-only copy of this history
    */
  def getReader: MempoolReader[TX] = this
}