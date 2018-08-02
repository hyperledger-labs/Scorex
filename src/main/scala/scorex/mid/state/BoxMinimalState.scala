package scorex.mid.state

import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{BoxStateChanges, MinimalState, ModifierValidation, TransactionValidation}
import scorex.core.{PersistentNodeViewModifier, VersionTag, idToVersion}

import scala.util.{Failure, Success, Try}


trait BoxMinimalState[P <: Proposition,
  BX <: Box[P],
  BTX <: BoxTransaction[P, BX],
  M <: PersistentNodeViewModifier,
  BMS <: BoxMinimalState[P, BX, BTX, M, BMS]]
  extends MinimalState[M, BMS] with TransactionValidation[BTX] with ModifierValidation[M] {
  self: BMS =>

  def closedBox(boxId: Array[Byte]): Option[BX]

  def boxesOf(proposition: P): Seq[BX]

  def changes(mod: M): Try[BoxStateChanges[P, BX]]

  def applyChanges(changes: BoxStateChanges[P, BX], newVersion: VersionTag): Try[BMS]

  override def applyModifier(mod: M): Try[BMS] = {
    validate(mod) flatMap {_ =>
      changes(mod).flatMap(cs => applyChanges(cs, idToVersion(mod.id)))
    }
  }

 override def validate(mod: M): Try[Unit]

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
    * @param tx - transaction to check against the state
    * @return
    */
  override def validate(tx: BTX): Try[Unit] = {
    val statefulValid = {
      val boxesSumTry = tx.unlockers.foldLeft[Try[Long]](Success(0L)) { case (partialRes, unlocker) =>
        partialRes.flatMap { partialSum =>
          closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              unlocker.boxKey.isValid(box.proposition, tx.messageToSign) match {
                case true => Success(partialSum + box.value)
                case false => Failure(new Exception("Incorrect unlocker"))
              }
            case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
          }
        }
      }

      boxesSumTry flatMap { openSum =>
        tx.newBoxes.map(_.value).sum == openSum - tx.fee match {
          case true => Success[Unit](Unit)
          case false => Failure(new Exception("Negative fee"))
        }
      }
    }
    statefulValid.flatMap(_ => semanticValidity(tx))
  }

  def semanticValidity(tx: BTX): Try[Unit]
}


