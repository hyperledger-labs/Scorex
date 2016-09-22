package scorex.core.block

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition

case class StateChanges[P <: Proposition, BX <: Box[P]](toRemove: Set[BX], toAppend: Set[BX])
