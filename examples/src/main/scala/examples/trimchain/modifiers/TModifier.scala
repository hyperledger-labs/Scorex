package examples.trimchain.modifiers

import examples.commons.SimpleBoxTransaction
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition


trait TModifier extends PersistentNodeViewModifier

object TModifier {
  val Header = 0: Byte
  val UtxoSnapshot = 1: Byte
  val Block = 2: Byte
}