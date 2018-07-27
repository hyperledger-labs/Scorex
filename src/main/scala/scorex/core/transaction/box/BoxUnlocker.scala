package scorex.core.transaction.box

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Proof
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADKey

trait BoxUnlocker[P <: Proposition] extends ScorexEncoding {
  val closedBoxId: ADKey
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${encoder.encode(closedBoxId)}, boxKey: $boxKey)"
}
