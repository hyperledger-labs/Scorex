package scorex.core.transaction.proof

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.signatures.Curve25519

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Array[Byte]) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition] {

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.bytes)
}

object Signature25519 {
  lazy val SignatureSize = Curve25519.SignatureLength25519
}
