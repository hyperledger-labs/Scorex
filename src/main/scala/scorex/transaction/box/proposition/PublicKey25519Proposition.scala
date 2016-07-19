package scorex.transaction.box.proposition

import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash._
import scorex.settings.SizedConstants
import shapeless.Sized

import scala.util.{Success, Failure, Try}

trait PublicKey25519Proposition extends PublicKeyProposition {

  import PublicKey25519Proposition._

  override type PublicKeySize = SizedConstants.Nat32

  override lazy val address: String = Base58.encode((AddressVersion +: id) ++ calcCheckSum(id))
}

object PublicKey25519Proposition {
  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val PubKeyLength = 32
  val AddressLength = 1 + PubKeyLength + ChecksumLength

  def apply(pubKey: Sized[Array[Byte], SizedConstants.PubKey25519]): PublicKey25519Proposition =
    new PublicKey25519Proposition {
      override val publicKey = pubKey
      override lazy val bytes: Array[Byte] = publicKey
    }

  //todo: unsized
  def calcCheckSum(bytes: Array[Byte]): Array[Byte] = hash(bytes).unsized.take(ChecksumLength)

  def validPubKey(address: String): Try[PublicKey25519Proposition] =
    Base58.decode(address).flatMap { addressBytes =>
      if (addressBytes.length != AddressLength)
        Failure(new Exception("Wrong address length"))
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

        if (checkSum.sameElements(checkSumGenerated))
          Success(PublicKey25519Proposition(Sized.wrap(addressBytes.dropRight(ChecksumLength).tail)))
        else Failure(new Exception("Wrong checksum"))
      }
    }
}