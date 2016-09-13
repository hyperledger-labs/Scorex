package scorex.core.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.state.MinimalState

import scala.util.{Failure, Success, Try}
import scorex.utils.toTry

trait NodeViewModifier {
  self =>

  import NodeViewModifier.{ModifierId, ModifierTypeId}

  type M >: self.type <: NodeViewModifier

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id(): ModifierId

  def companion: NodeViewModifierCompanion[M]
}

trait NodeViewModifierCompanion[M <: NodeViewModifier] {
  def bytes(modifier: M): Array[Byte]

  def parse(bytes: Array[Byte]): Try[M]

  def bytes(modifiers: Seq[M]): Seq[Array[Byte]] = modifiers.map(bytes)
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * are of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  type ModifierTypeId = Byte
  type ModifierId = Array[Byte]

  val ModifierIdSize: Int = 32 //todo: make configurable via application.conf
}

trait PersistentNodeViewModifier[P <: Proposition, TX <: Transaction[P, TX]] extends NodeViewModifier {

  // with Dotty is would be Seq[TX] | Nothing
  def transactions: Option[Seq[TX]]
}

case class TransactionChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]], minerReward: Long)


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition, TX <: Transaction[P, TX]]
  extends NodeViewModifier with JsonSerializable {

  override val modifierTypeId: Byte = Transaction.TransactionModifierId

  val fee: Long

  val timestamp: Long

  def json: Json

  def validate(state: MinimalState[P, TX, _, _]): Try[Unit]

  val messageToSign: Array[Byte]

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(state: MinimalState[P, TX, _, _]): Try[TransactionChanges[P]]
}

object Transaction {
  val TransactionModifierId = 2: Byte
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
  override def validate(state: MinimalState[P, BoxTransaction[P], _, _]): Try[Unit] = {
    lazy val statelessValid = toTry(fee >= 0, "Negative fee")

    lazy val statefulValid = {
      val boxesSumTry = unlockers.foldLeft[Try[Long]](Success(0L)) { case (partialRes, unlocker) =>
        partialRes.flatMap { partialSum =>
          state.closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              unlocker.boxKey.isValid(box.proposition, messageToSign) match {
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
    statefulValid.flatMap(_ => statelessValid).flatMap(_ => semanticValidity)
  }

  def semanticValidity: Try[Unit]
}