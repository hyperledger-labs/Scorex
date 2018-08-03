package scorex.core.transaction

import scorex.core.{EphemerealNodeViewModifier, ModifierTypeId}
import scorex.crypto.hash.Blake2b256
import scorex.util._


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId = Transaction.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = bytesToId(Blake2b256(messageToSign))
}


object Transaction {
  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}
