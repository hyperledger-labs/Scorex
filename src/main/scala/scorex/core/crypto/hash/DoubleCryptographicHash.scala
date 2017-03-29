package scorex.core.crypto.hash

import scorex.crypto.hash.{Blake2b256, CryptographicHash, Keccak256}

/**
  * The chain of two hash functions, Blake and Keccak
  */

object DoubleCryptographicHash extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = Keccak256(Blake2b256(in))
}
