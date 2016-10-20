package scorex.core.transaction

import io.circe.Json
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.Box


case class TransactionChanges[P <: Proposition, BX <: Box[P]](toRemove: Set[BX], toAppend: Set[BX], minerReward: Long)


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition]
  extends NodeViewModifier with JsonSerializable {

  override val modifierTypeId: Byte = Transaction.TransactionModifierId

  val fee: Long

  val timestamp: Long

  def json: Json

}


object Transaction {
  type TransactionId = NodeViewModifier.ModifierId

  val TransactionModifierId = 2: Byte
}