package scorex.transaction.proof

import scorex.transaction.box.EmptyProposition

case object NoProof extends Proof[EmptyProposition] {
  override val proofId = 1: Byte
  override val proofBytes: Array[Byte] = Array()

  override def isValid(proposition: EmptyProposition, message: Array[Byte]): Boolean = true
}
