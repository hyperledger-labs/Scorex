package scorex.transaction.proof

import scorex.crypto.signatures.Curve25519
import scorex.transaction.box.PublicKey25519Proposition

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Array[Byte]) extends Proof[PublicKey25519Proposition] {
  override def proofId: Byte = 100: Byte

  override def proofBytes: Array[Byte] = signature

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.publicKey)
}

object Signature25519 {
  lazy val SignatureSize = Curve25519.SignatureLength25519
}