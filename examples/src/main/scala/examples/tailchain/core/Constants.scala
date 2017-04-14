package examples.tailchain.core

import scorex.crypto.hash.Blake2b256


object Constants {
  val n = 20
  val k = 1
  val NElementsInProof = 2

  val hashfn = Blake2b256

  type StateRoot = Array[Byte]
  val StateRootLength = hashfn.DigestSize

  type TransactionsRoot = Array[Byte]
  val TxRootLength = hashfn.DigestSize


  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("10")
}