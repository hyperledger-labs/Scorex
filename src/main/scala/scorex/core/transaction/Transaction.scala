package scorex.core.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.NodeViewModifier
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}


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

  val messageToSign: Array[Byte]
}

object Transaction {
  type TransactionId = NodeViewModifier.ModifierId

  val TransactionModifierId = 2: Byte
}

abstract class BoxTransaction[P <: Proposition, BX <: Box[P]] extends Transaction[P] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  override lazy val messageToSign: Array[Byte] =
    newBoxes.map(_.bytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
}