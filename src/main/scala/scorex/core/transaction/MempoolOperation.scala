package scorex.core.transaction

import scorex.core.NodeViewComponentOperation
import scorex.core.NodeViewComponentOperation.OperationMode

import scala.util.Try

/** Base trait for all memory pool operations, which should be sent to a memory pool actor
  */
trait MempoolOperation extends NodeViewComponentOperation

/** Messages for the MemoryPool actor
  */
object MempoolOperation {

  /** Put transactions into the memory pool. Validation of the transactions against
    * the state is done in NodeViewHolder. This put() method can check whether a transaction is valid.
    * Returns a `PutResponse`
    */
  case class Put[TX <: Transaction](transaction: TX, mode: OperationMode[Put[_]] = PutNormal)
    extends MempoolOperation

  /** Result of the `Put` operation. Should not be sent in `PutWithoutCheck` mode
    */
  case class PutResponse[TX <: Transaction](transaction: TX, result: Try[Unit], mode: OperationMode[Put[_]])

  /** Remove transaction from memory pool
    */
  case class Remove[TX <: Transaction](transaction: TX) extends MempoolOperation

  /** Remove transaction that complies criteria
    */
  case class RemoveBy[TX <: Transaction](criteria: TX => Boolean)

  /** Normal Put operation
    */
  object PutNormal extends OperationMode[Put[_]]

  /** Memory pool should not send back the result of Put operation
    */
  object PutWithoutCheck extends OperationMode[Put[_]]{
    def apply[TX <: Transaction](tx: TX): Put[TX] = Put(tx, PutWithoutCheck)
  }

}
