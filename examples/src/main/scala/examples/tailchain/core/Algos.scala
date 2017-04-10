package examples.tailchain.core

import scala.util.Random


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
