package examples.tailchain.core
import scorex.crypto.hash.Blake2b256

import scala.util.Random


object Constants {
  val n = 1000
  val k = 2

  val hashfn = Blake2b256

  type StateRoot = Array[Byte]
  val stateRootLength = hashfn.DigestSize

  type TransactionsRoot = Array[Byte]
  val txRootLength = hashfn.DigestSize
}

object Algos extends App {

  import Constants._

  def chooseSnapshots(chainLength: Int, publicKey: Array[Byte]): Seq[Int] = {
    (1 to k).map { i =>
      val h = BigInt(1, hashfn(publicKey ++ Array(i.toByte))).mod(n).toInt + (chainLength - n)
      if (h > 0) h else 1
    }.sorted
  }

  println(chooseSnapshots(9000, Array.fill(32)(Random.nextInt(100).toByte)))
}
