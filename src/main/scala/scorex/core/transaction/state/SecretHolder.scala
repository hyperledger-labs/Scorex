package scorex.core.transaction.state

import scorex.core.crypto.encode.Base58
import scorex.core.crypto.signatures.Curve25519
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box._
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicImage, PublicKey25519Proposition}
import scorex.core.transaction.proof.{ProofOfKnowledge, Signature25519}
import scala.util.Try

trait Secret extends BytesSerializable {
  self =>
  type S >: self.type <: Secret
  type PK <: PublicImage[S]

  def companion: SecretCompanion[S]

  def instance: S = self

  def publicImage: PK
}

trait SecretCompanion[S <: Secret] {
  type PK = S#PK

  type PR <: ProofOfKnowledge[S, _ <: ProofOfKnowledgeProposition[S]]

  def owns(secret: S, box: Box[_]): Boolean

  def sign(secret: S, message: Array[Byte]): PR

  def verify(message: Array[Byte], publicImage: PK, proof: PR): Boolean

  def generateKeys(randomSeed: Array[Byte]): (S, PK)
}

case class PrivateKey25519(privKeyBytes: Array[Byte], publicKeyBytes: Array[Byte]) extends Secret {
  override type S = PrivateKey25519
  override type PK = PublicKey25519

  override lazy val companion: SecretCompanion[PrivateKey25519] = PrivateKey25519Companion

  override lazy val bytes: Array[Byte] = privKeyBytes

  override lazy val publicImage: PublicKey25519 = PublicKey25519(publicKeyBytes)
}

case class PublicKey25519(publicKeyBytes: Array[Byte]) extends PublicImage[PrivateKey25519] {
  override def id: Array[Byte] = publicKeyBytes

  override def address: String = Base58.encode(publicKeyBytes)

  override def bytes: Array[Byte] = publicKeyBytes
}

object PrivateKey25519Companion extends SecretCompanion[PrivateKey25519] {

  override type PR = Signature25519

  //todo: implement
  override def owns(secret: PrivateKey25519, box: Box[_]): Boolean = {
    box.proposition match {
      case _ => ???
    }
  }

  override def sign(secret: PrivateKey25519, message: Array[Byte]): Signature25519 =
    Signature25519(Curve25519.sign(secret.bytes, message))

  override def verify(message: Array[Byte], publicImage: PublicKey25519, proof: Signature25519): Boolean =
    Curve25519.verify(proof.signature, message, publicImage.bytes)

  override def generateKeys(randomSeed: Array[Byte]): (PrivateKey25519, PublicKey25519) = {
    val pair = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = PrivateKey25519(pair._1, pair._2)
    secret -> secret.publicImage
  }

  def parseBytes(bytes: Array[Byte]): Try[(PrivateKey25519, PublicKey25519)] = Try {
    val privKey = PrivateKey25519(bytes.slice(0, 32), bytes.slice(32, 64))
    privKey -> privKey.publicImage
  }

  def keyPairBytes(priv: PrivateKey25519): Array[Byte] =
    priv.bytes ++ priv.publicImage.bytes
}