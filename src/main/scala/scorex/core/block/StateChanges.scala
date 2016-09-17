package scorex.core.block

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition

case class StateChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]])
