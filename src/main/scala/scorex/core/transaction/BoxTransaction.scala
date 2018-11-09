package scorex.core.transaction

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.newserialization.ScorexSerializer
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}


abstract class BoxTransaction[P <: Proposition, BX <: Box[P]] extends Transaction {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  val fee: Long

  val timestamp: Long

//  override lazy val messageToSign: Array[Byte] = {
//    Bytes.concat(if (newBoxes.nonEmpty) scorex.core.utils.concatBytes(newBoxes.map(_.bytes)) else Array[Byte](),
//      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
//      Longs.toByteArray(timestamp),
//      Longs.toByteArray(fee))
//  }
}
