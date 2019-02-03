package scorex.core.transaction.proof

import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.{Curve25519, Signature}

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Signature) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition]
  with ScorexEncoding {

  require(signature.isEmpty || signature.length == Curve25519.SignatureLength,
    s"${signature.length} != ${Curve25519.SignatureLength}")

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.pubKeyBytes)

  override type M = Signature25519

  override def serializer: ScorexSerializer[Signature25519] = Signature25519Serializer

  override def toString: String = s"Signature25519(${encoder.encode(signature)})"
}

object Signature25519Serializer extends ScorexSerializer[Signature25519] {

  override def serialize(obj: Signature25519, w: Writer): Unit = {
    w.putBytes(obj.signature)
  }

  override def parse(r: Reader): Signature25519 = {
    Signature25519(Signature @@ r.getBytes(Curve25519.SignatureLength))
  }
}

object Signature25519 {
  lazy val SignatureSize = Curve25519.SignatureLength
}
