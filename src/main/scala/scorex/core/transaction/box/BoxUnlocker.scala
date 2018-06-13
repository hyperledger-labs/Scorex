package scorex.core.transaction.box

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Proof
import scorex.core.utils.ScorexEncoding

trait BoxUnlocker[P <: Proposition] extends ScorexEncoding {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${encoder.encode(closedBoxId)}, boxKey: $boxKey)"
}
