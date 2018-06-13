package scorex.core.transaction.proof

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.signatures.{Curve25519, Signature}

import scala.util.Try

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Signature) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition]
  with ScorexLogging {

  require(signature.isEmpty || signature.length == Curve25519.SignatureLength,
    s"${signature.length} != ${Curve25519.SignatureLength}")

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.pubKeyBytes)

  override type M = Signature25519

  override def serializer: Serializer[Signature25519] = Signature25519Serializer

  override def toString: String = s"Signature25519(${encoder.encode(signature)})"
}

object Signature25519Serializer extends Serializer[Signature25519] {
  override def toBytes(obj: Signature25519): Array[Byte] = obj.signature

  override def parseBytes(bytes: Array[Byte]): Try[Signature25519] = Try(Signature25519(Signature @@ bytes))
}

object Signature25519 {
  lazy val SignatureSize = Curve25519.SignatureLength
}
