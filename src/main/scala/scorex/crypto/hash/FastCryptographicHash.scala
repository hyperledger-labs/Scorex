package scorex.crypto.hash

import scorex.crypto.hash.CryptographicHash._
import shapeless.Sized
import scorex.settings.SizedConstants.Nat32

/**
  * Interface for fast and secure Blake hash function
  */

object FastCryptographicHash {

  type Digest = Sized[Array[Byte], Nat32]

  def apply(input: Message): Digest = hash(input)

  def apply(input: String): Digest = hash(input.getBytes)

  def hash(in: Message): Digest = Sized.wrap(Blake2b256.hash(in))

}
