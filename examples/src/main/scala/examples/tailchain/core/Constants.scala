package examples.tailchain.core

import scorex.crypto.hash.Blake2b256


object Constants {
  val n = 1000
  val k = 2

  val hashfn = Blake2b256

  type StateRoot = Array[Byte]
  val stateRootLength = hashfn.DigestSize

  type TransactionsRoot = Array[Byte]
  val txRootLength = hashfn.DigestSize
}