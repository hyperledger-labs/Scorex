package scorex.core.transaction.box.proposition

import scorex.core.serialization.Serializer
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class PublicKey25519Proposition(pubKeyBytes: PublicKey) extends ProofOfKnowledgeProposition[PrivateKey25519] {

  require(pubKeyBytes.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found")

  import PublicKey25519Proposition._

  private def bytesWithVersion: Array[Byte] = AddressVersion +: pubKeyBytes

  lazy val address: String = Base58.encode(bytesWithVersion ++ calcCheckSum(bytesWithVersion))

  override def toString: String = address

  def verify(message: Array[Byte], signature: Signature): Boolean = Curve25519.verify(signature, message, pubKeyBytes)

  override type M = PublicKey25519Proposition

  override def serializer: Serializer[PublicKey25519Proposition] = PublicKey25519PropositionSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: PublicKey25519Proposition => p.pubKeyBytes sameElements pubKeyBytes
    case _ => false
  }

  override def hashCode(): Int = (BigInt(pubKeyBytes) % Int.MaxValue).toInt

}

object PublicKey25519PropositionSerializer extends Serializer[PublicKey25519Proposition] {
  override def toBytes(obj: PublicKey25519Proposition): Seq[Byte] = obj.pubKeyBytes

  override def parseBytes(bytes: Seq[Byte]): Try[PublicKey25519Proposition] =
    Try(PublicKey25519Proposition(PublicKey @@ bytes.toArray))
}

object PublicKey25519Proposition {
  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val AddressLength = 1 + Constants25519.PubKeyLength + ChecksumLength

  def calcCheckSum(bytes: Array[Byte]): Array[Byte] = Blake2b256.hash(bytes).take(ChecksumLength)

  def validPubKey(address: String): Try[PublicKey25519Proposition] =
    Base58.decode(address).flatMap { addressBytes =>
      if (addressBytes.length != AddressLength)
        Failure(new Exception("Wrong address length"))
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

        if (checkSum.sameElements(checkSumGenerated))
          Success(PublicKey25519Proposition(PublicKey @@ addressBytes.dropRight(ChecksumLength).tail))
        else Failure(new Exception("Wrong checksum"))
      }
    }
}

object Constants25519 {
  val PrivKeyLength = 32
  val PubKeyLength = 32
}