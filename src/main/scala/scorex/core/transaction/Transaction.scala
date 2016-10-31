package scorex.core.transaction

import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition


case class TransactionChanges[P <: Proposition, BX <: Box[P]](toRemove: Set[BX], toAppend: Set[BX], minerReward: Long)


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: Byte = Transaction.ModifierTypeId

  val fee: Long

  val timestamp: Long

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = FastCryptographicHash(messageToSign)
}


object Transaction {
  val ModifierTypeId = 2: Byte
  type TransactionId = NodeViewModifier.ModifierId
}