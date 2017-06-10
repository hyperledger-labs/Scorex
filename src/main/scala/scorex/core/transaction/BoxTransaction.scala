package scorex.core.transaction

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{BoxUnlocker, Box}


abstract class BoxTransaction[P <: Proposition, T, BX <: Box[P, T]] extends Transaction[P] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  override lazy val messageToSign: Array[Byte] =
    Bytes.concat(if (newBoxes.nonEmpty) scorex.core.utils.concatBytes(newBoxes.map(_.bytes)) else Array[Byte](),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
}
