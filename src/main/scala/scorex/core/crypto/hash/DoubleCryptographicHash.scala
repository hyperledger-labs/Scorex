package scorex.core.crypto.hash

import scorex.crypto.applyHashes
import scorex.crypto.hash.{Blake256, CryptographicHash, Keccak256}

/**
  * The chain of two hash functions, Blake and Keccak
  */

object DoubleCryptographicHash extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = applyHashes(in, Blake256, Keccak256)
}
