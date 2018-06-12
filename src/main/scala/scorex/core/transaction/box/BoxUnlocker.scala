package scorex.core.transaction.box

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Proof
import scorex.core.utils.ScorexLogging

trait BoxUnlocker[P <: Proposition] extends ScorexLogging {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${encoder.encode(closedBoxId)}, boxKey: $boxKey)"
}
