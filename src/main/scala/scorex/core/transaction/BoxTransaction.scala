package scorex.core.transaction

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}


abstract class BoxTransaction[P <: Proposition, BX <: Box[P]] extends Transaction {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  val fee: Long

  val timestamp: Long

}
