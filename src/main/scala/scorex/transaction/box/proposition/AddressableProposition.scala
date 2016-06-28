package scorex.transaction.box.proposition

trait AddressableProposition extends Proposition {
  val id: Array[Byte]
  val address: String
}
