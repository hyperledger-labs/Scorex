package scorex.core.transaction.box

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Proof

trait BoxUnlocker[P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]
}
