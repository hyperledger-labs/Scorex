package scorex.core.transaction

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}


abstract class BoxTransaction[P <: Proposition, BX <: Box[P]] extends Transaction[P] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  override lazy val messageToSign: Array[Byte] =
    Bytes.concat(if (newBoxes.nonEmpty) newBoxes.map(_.bytes).reduce(_ ++ _) else Array[Byte](),
      unlockers.map(_.closedBoxId).reduce(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
}
