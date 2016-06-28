package scorex.transaction.box.proposition

import scorex.crypto.signatures.Curve25519
import scorex.settings.SizedConstants
import shapeless.{Sized, Nat}

trait PublicKeyProposition extends AddressableProposition {
  type PublicKeySize <: Nat
  val publicKey: Sized[Array[Byte], PublicKeySize]

  override val bytes = publicKey.unsized


  override def toString: String = address

  override lazy val id: Array[Byte] = publicKey.unsized

  def verify(message: Array[Byte], signature: Sized[Array[Byte], SizedConstants.Signature25519]): Boolean =
    Curve25519.verify(signature, message, publicKey)
}
