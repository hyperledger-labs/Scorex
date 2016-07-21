package scorex.crypto.hash

import ove.crypto.digest.Blake2b

/**
 * Interface for fast and secure Blake hash function
 */

object FastCryptographicHash extends CryptographicHash32 {

  override def hash(input: Message): Digest = Blake2b256.hash(input)

}
