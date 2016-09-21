package scorex.core.transaction.state.authenticated

import scorex.crypto.authds.storage.StorageType
import scorex.crypto.authds.{AuthenticatedDictionary, DataProof}
import scorex.crypto.hash.CryptographicHash
import scorex.core.transaction.{BoxTransaction, PersistentNodeViewModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.utils._

import scala.util.{Failure, Success, Try}


trait BoxMinimalState[P <: Proposition, BTX <: BoxTransaction[P], M <: PersistentNodeViewModifier[P, BTX], BMS <: BoxMinimalState[P, BTX, M, BMS]]
  extends MinimalState[P, BTX, M, BMS] {
  self: BMS =>

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
    lazy val statelessValid = toTry(tx.fee >= 0, "Negative fee")

    lazy val statefulValid = {
      val boxesSumTry = tx.unlockers.foldLeft[Try[Long]](Success(0L)) { case (partialRes, unlocker) =>
        partialRes.flatMap { partialSum =>
          closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              unlocker.boxKey.isValid(box.proposition, tx.messageToSign) match {
                case true => Success(partialSum + box.value)
                case false => Failure(new Exception(""))
              }
            case None => Failure(new Exception(""))
          }
        }
      }

      boxesSumTry flatMap { openSum =>
        tx.newBoxes.map(_.value).sum == openSum - tx.fee match {
          case true => Success[Unit](Unit)
          case false => Failure(new Exception(""))
        }
      }
    }
    statefulValid.flatMap(_ => statelessValid).flatMap(_ => semanticValidity(tx))
  }

  def semanticValidity(tx: BTX): Try[Unit]
}

trait AuthenticatedBoxMinimalState[P <: Proposition, TX <: Transaction[P], M <: PersistentNodeViewModifier[P, TX], HashFunction <: CryptographicHash,
AMS <: AuthenticatedBoxMinimalState[P, TX, M, HashFunction, AMS]]
  extends MinimalState[P, TX, M, AMS] {
  self: AMS =>

  type ElementProof <: DataProof
  type Storage <: StorageType

  protected val boxesStorage: AuthenticatedDictionary[ElementProof, Storage]

  def digest: HashFunction#Digest

  val hashFunction: HashFunction
}
