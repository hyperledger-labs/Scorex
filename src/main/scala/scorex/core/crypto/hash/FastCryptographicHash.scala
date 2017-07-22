package scorex.core.crypto.hash

import scorex.crypto.hash.{Blake2b256, CryptographicHash32}

/**
 * Interface for Blake2b hash function
 */

object FastCryptographicHash extends CryptographicHash32 {

  override def hash(input: Message): Digest = Blake2b256.hash(input)

}
