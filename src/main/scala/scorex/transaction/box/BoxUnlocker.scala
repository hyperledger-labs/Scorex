package scorex.transaction.box

import scorex.transaction.proof.Proof

trait BoxUnlocker[P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]
}