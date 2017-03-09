package scorex.core.transaction.box.proposition

import scorex.core.crypto.hash.FastCryptographicHash._
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success, Try}

case class PublicKey25519Proposition(pubKeyBytes: Array[Byte]) extends ProofOfKnowledgeProposition[PrivateKey25519] {

  require(pubKeyBytes.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found")

  import PublicKey25519Proposition._

  private def bytesWithVersion: Array[Byte] = AddressVersion +: pubKeyBytes

  lazy val address: String = Base58.encode(bytesWithVersion ++ calcCheckSum(bytesWithVersion))

  override def toString: String = address

  def verify(message: Array[Byte], signature: Array[Byte]): Boolean = Curve25519.verify(signature, message, pubKeyBytes)

  override type M = PublicKey25519Proposition

  override def serializer: Serializer[PublicKey25519Proposition] = PublicKey25519PropositionSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: PublicKey25519Proposition => p.pubKeyBytes sameElements pubKeyBytes
    case _ => false
  }
}

object PublicKey25519PropositionSerializer extends Serializer[PublicKey25519Proposition] {
  override def toBytes(obj: PublicKey25519Proposition): Array[Byte] = obj.pubKeyBytes

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519Proposition] = Try(PublicKey25519Proposition(bytes))
}

object PublicKey25519Proposition {
  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val AddressLength = 1 + Constants25519.PubKeyLength + ChecksumLength

  def calcCheckSum(bytes: Array[Byte]): Array[Byte] = hash(bytes).take(ChecksumLength)

  def validPubKey(address: String): Try[PublicKey25519Proposition] =
    Base58.decode(address).flatMap { addressBytes =>
      if (addressBytes.length != AddressLength)
        Failure(new Exception("Wrong address length"))
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

        if (checkSum.sameElements(checkSumGenerated))
          Success(PublicKey25519Proposition(addressBytes.dropRight(ChecksumLength).tail))
        else Failure(new Exception("Wrong checksum"))
      }
    }
}

object Constants25519 {
  val PrivKeyLength = 32
  val PubKeyLength = 32
}