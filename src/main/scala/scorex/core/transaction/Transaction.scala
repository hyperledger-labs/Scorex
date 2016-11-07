package scorex.core.transaction

import io.circe.Json
import scorex.core.NodeViewModifier
import scorex.core.transaction.box.proposition.Proposition


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: Byte = Transaction.ModifierTypeId

  val fee: Long

  val timestamp: Long

  def json: Json

}


object Transaction {
  val ModifierTypeId = 2: Byte
  type TransactionId = NodeViewModifier.ModifierId
}