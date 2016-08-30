package scorex.core.transaction.proof

import scorex.core.crypto.signatures.Curve25519
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Array[Byte]) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition] {
  override lazy val bytes: Array[Byte] = signature

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.publicKey.bytes)
}

object Signature25519 {
  lazy val SignatureSize = Curve25519.SignatureLength25519
}
