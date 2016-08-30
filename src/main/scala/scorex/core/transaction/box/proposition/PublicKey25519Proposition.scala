package scorex.core.transaction.box.proposition

import scorex.crypto.encode.Base58
import scorex.core.crypto.hash.FastCryptographicHash._
import scorex.crypto.signatures.Curve25519
import scorex.core.settings.SizedConstants
import scorex.core.transaction.state.{PrivateKey25519, PublicKey25519}
import shapeless.Sized

import scala.util.{Failure, Success, Try}

case class PublicKey25519Proposition(publicKey: PublicKey25519) extends ProofOfKnowledgeProposition[PrivateKey25519] {
  import PublicKey25519Proposition._

  lazy val pubKeyBytes = publicKey.bytes

  lazy val address: String = Base58.encode((AddressVersion +: pubKeyBytes) ++ calcCheckSum(pubKeyBytes))

  lazy val bytes = publicKey.bytes

  override def toString: String = address

  def verify(message: Array[Byte], signature: Sized[Array[Byte], SizedConstants.Signature25519]): Boolean =
    Curve25519.verify(signature, message, pubKeyBytes)
}


object PublicKey25519Proposition {
  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val PubKeyLength = 32
  val AddressLength = 1 + PubKeyLength + ChecksumLength

  //todo: unsized
  def calcCheckSum(bytes: Array[Byte]): Array[Byte] = hash(bytes).take(ChecksumLength)

  def validPubKey(address: String): Try[PublicKey25519Proposition] =
    Base58.decode(address).flatMap { addressBytes =>
      if (addressBytes.length != AddressLength)
        Failure(new Exception("Wrong address length"))
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

        if (checkSum.sameElements(checkSumGenerated))
          Success(PublicKey25519Proposition(PublicKey25519(addressBytes.dropRight(ChecksumLength).tail)))
        else Failure(new Exception("Wrong checksum"))
      }
    }
}