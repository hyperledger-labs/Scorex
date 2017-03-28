package examples.tailchain.modifiers

import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.box.proposition.PublicKey25519Proposition


trait TModifier extends PersistentNodeViewModifier[PublicKey25519Proposition, SimpleBoxTransaction]

object TModifier {
  val Header = 0: Byte
  val UtxoSnapshot = 1: Byte
  val Block = 2: Byte
}