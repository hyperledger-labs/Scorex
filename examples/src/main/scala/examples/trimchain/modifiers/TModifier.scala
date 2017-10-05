package examples.trimchain.modifiers

import scorex.core._


trait TModifier extends PersistentNodeViewModifier

object TModifier {
  val Header: ModifierTypeId = ModifierTypeId @@ 0.toByte
  val UtxoSnapshot: ModifierTypeId = ModifierTypeId @@ 1.toByte
  val Block: ModifierTypeId = ModifierTypeId @@ 2.toByte
}