package scorex.transaction.box

import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash._
import scorex.crypto.signatures.Curve25519
import scorex.serialization.BytesSerializable
import shapeless.{Nat, Sized, Succ}

import scala.util.{Failure, Success, Try}

sealed trait Proposition extends BytesSerializable

sealed trait AddressableProposition extends Proposition {
  val id: Array[Byte]
  val address: String
}

trait EmptyProposition extends Proposition

trait PublicKeyProposition extends AddressableProposition {
  type PublicKeySize <: Nat
  val publicKey: Sized[Array[Byte], PublicKeySize]

  override val bytes = publicKey.unsized


  override def toString: String = address

  override lazy val id: Array[Byte] = publicKey.unsized

  def verify(message: Array[Byte], signature: Sized[Array[Byte], SizedConstants.Signature25519]): Boolean =
    Curve25519.verify(signature, message, publicKey)
}


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
      override val bytes: Array[Byte] = publicKey
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

object SizedConstants {
  type Nat32 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat._22]]]]]]]]]]

  type Nat40 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat32]]]]]]]]

  type Nat50 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat40]]]]]]]]]]

  type Nat60 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat50]]]]]]]]]]

  type Nat64 = Succ[Succ[Succ[Succ[Nat60]]]]

  type PrivKey25519 = Nat32

  type PubKey25519 = Nat32

  type Blake2bDigestSize = Nat32

  type Signature25519 = Nat64
}

//todo: a_reserve
//todo: sigma protocol id
sealed trait SigmaProposition extends AddressableProposition {
  val a: Array[Byte]
  val bytes = a
}

case class HeightOpenProposition(height: Int) extends Proposition {
  override val bytes = Ints.toByteArray(height)
}