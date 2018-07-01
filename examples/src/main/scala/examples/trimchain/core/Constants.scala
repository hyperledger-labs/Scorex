package examples.trimchain.core

import scorex.core.crypto.hash.Blake2b256


object Constants {
  val n: Int = 20
  val k: Int = 1
  val NElementsInProof: Int = 10

  val hashfn: Blake2b256.type = Blake2b256

  val StateRootLength: Int = hashfn.DigestSize

  val TxRootLength: Int = hashfn.DigestSize


  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("2")
}