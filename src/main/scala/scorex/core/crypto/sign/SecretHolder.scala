package scorex.core.crypto.sign

import scorex.core.crypto.{Secret, SecretCompanion}
import scorex.core.transaction.box._
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.proof.{ProofOfKnowledge, Signature25519}
import scorex.crypto.signatures.Curve25519




case class PrivateKey25519(privKeyBytes: Array[Byte], publicKeyBytes: Array[Byte]) extends Secret {
  override type S = PrivateKey25519
  override type PK = PublicKey25519Proposition

  override lazy val companion: SecretCompanion[PrivateKey25519] = PrivateKey25519Companion

  override lazy val publicImage: PublicKey25519Proposition = PublicKey25519Proposition(publicKeyBytes)
}

object PrivateKey25519Companion extends SecretCompanion[PrivateKey25519] {

  override type PR = Signature25519

  override def owns(secret: PrivateKey25519, box: Box[_]): Boolean = {
    box.proposition match {
      case p: PublicKey25519Proposition => p.pubKeyBytes sameElements secret.publicKeyBytes
      case _ => false
    }
  }

  override def sign(secret: PrivateKey25519, message: Array[Byte]): Signature25519 =
    Signature25519(Curve25519.sign(secret.privKeyBytes, message))

  override def verify(message: Array[Byte], publicImage: PublicKey25519Proposition, proof: Signature25519): Boolean =
    proof.isValid(publicImage, message)

  override def generateKeys(randomSeed: Array[Byte]): (PrivateKey25519, PublicKey25519Proposition) = {
    val pair = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = PrivateKey25519(pair._1, pair._2)
    secret -> secret.publicImage
  }
}