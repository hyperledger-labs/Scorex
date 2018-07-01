package scorex.core.transaction

import scorex.core.{EphemerealNodeViewModifier, ModifierId, ModifierTypeId}
import scorex.core.crypto.hash.Blake2b256


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId = Transaction.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = ModifierId @@ Blake2b256(messageToSign)
}


object Transaction {
  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}