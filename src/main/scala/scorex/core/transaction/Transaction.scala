package scorex.core.transaction

import scorex.core.{EphemerealNodeViewModifier, NodeViewModifier}
import scorex.core.NodeViewModifier._
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.hash.Blake2b256


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition] extends EphemerealNodeViewModifier {
  override val modifierTypeId: Byte = Transaction.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = Blake2b256(messageToSign)
}


object Transaction {
  val ModifierTypeId: ModifierTypeId = 2: Byte
  type TransactionId = NodeViewModifier.ModifierId
}