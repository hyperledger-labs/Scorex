package examples.spv

import scorex.crypto.hash.Blake2b256

object Constants {
  val hashfn: Blake2b256.type = Blake2b256

  lazy val MaxTarget: BigInt = BigInt(1, Array.fill(32)(Byte.MinValue))
  val InitialDifficulty: BigInt = BigInt(1)
}