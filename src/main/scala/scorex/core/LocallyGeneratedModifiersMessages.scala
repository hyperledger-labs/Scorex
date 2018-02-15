package scorex.core

import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

// Messages shared by LocalInterface and NodeViewHolder
object LocallyGeneratedModifiersMessages {
  object ReceivableMessages {
    case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)
    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)
  }
}
