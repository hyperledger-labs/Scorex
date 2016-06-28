package scorex.transaction.proof

import scorex.transaction.box.Proposition

case class AndProofs[P <: Proposition](proofs: (Proof[P], Proof[P])) extends Proof[P] {
  override val proofId = 20: Byte
  override lazy val proofBytes = proofs._1.bytes ++ proofs._2.bytes

  override def isValid(proposition: P, message: Array[Byte]): Boolean =
    proofs._1.isValid(proposition, message) && proofs._2.isValid(proposition, message)
}