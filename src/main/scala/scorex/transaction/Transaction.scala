package scorex.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.box.{Box, BoxUnlocker, Proposition}
import scorex.transaction.state.MinimalState

import scala.util.{Failure, Success, Try}


case class StateChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]], minerReward: Long)


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition, TX <: Transaction[P, TX]] extends BytesSerializable with JsonSerializable {

  def fee: Long

  val timestamp: Long

  /**
    * A transaction could be serialized into JSON
    * A Transaction opens existing boxes and creates new ones
    */
  def json: Json

  def validate(state: MinimalState[P, TX]): Try[Unit]

  def changes(state: MinimalState[P, TX]): Try[StateChanges[P]]

  //todo: move to some utils class
  protected def toTry(b: Boolean, msg: String): Try[Unit] = b match {
    case true => Success(Unit)
    case false => Failure(new Exception(msg))
  }

  val messageToSign: Array[Byte]
}

abstract class BoxTransaction[P <: Proposition] extends Transaction[P, BoxTransaction[P]] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[Box[P]]

  override lazy val messageToSign: Array[Byte] =
    newBoxes.map(_.bytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)

  /**
    * A transaction is valid against a state if:
    * - boxes a transaction is opening are stored in the state as closed
    * - sum of values of closed boxes = sum of values of open boxes - fee
    * - all the signatures for open boxes are valid(against all the txs bytes except of sigs)
    *
    * - fee >= 0
    *
    * specific semantic rules are applied
    *
    * @param state - state to check a transaction against
    * @return
    */
  override def validate(state: MinimalState[P, BoxTransaction[P]]): Try[Unit] = {
    lazy val statelessValid = toTry(fee >= 0, "Negative fee")

    lazy val statefulValid = {
      val boxesSumTry = unlockers.foldLeft[Try[Long]](Success(0L)) { case (partialRes, unlocker) =>
        partialRes.flatMap { partialSum =>
          state.closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              unlocker.boxKey.isValid(box.lock, messageToSign) match {
                case true => Success(partialSum + box.value)
                case false => Failure(new Exception(""))
              }
            case None => Failure(new Exception(""))
          }
        }
      }

      boxesSumTry flatMap { openSum =>
        newBoxes.map(_.value).sum == openSum - fee match {
          case true => Success[Unit](Unit)
          case false => Failure(new Exception(""))
        }
      }
    }
    statefulValid orElse statelessValid orElse semanticValidity
  }

  def semanticValidity: Try[Unit]
}