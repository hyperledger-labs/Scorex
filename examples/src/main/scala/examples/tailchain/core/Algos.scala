package examples.tailchain.core

import com.google.common.primitives.{Ints, Longs}
import examples.tailchain.utxo.AuthenticatedUtxo

import scala.util.{Random, Try}


object Algos extends App {

  import Constants._

  def chooseSnapshots(chainLength: Int, publicKey: Array[Byte]): Seq[Int] = {
    (1 to k).map { i =>
      val h = BigInt(1, hashfn(publicKey ++ Array(i.toByte))).mod(n).toInt + (chainLength - n)
      if (h > 0) h else 1
    }.sorted
  }

  def generateTicket(utxos: IndexedSeq[AuthenticatedUtxo],
                     st: Array[Byte],
                     minerPubKey: Array[Byte],
                     ctr: Long): Try[Ticket] = {
    require(utxos.size == k)

    var seed = hashfn(Longs.toByteArray(ctr))

    val partialProofs = utxos.zipWithIndex.map { case (utxo, idx) =>
      val id = hashfn(seed ++ minerPubKey ++ Ints.toByteArray(idx))
      val proof = utxo.lookupProof(id).get
      seed = id
      PartialProof(id, utxo.rootHash, pr, proof)
    }

    Ticket(minerPubKey, ctr, partialProofs)
  }

  println(chooseSnapshots(9000, Array.fill(32)(Random.nextInt(100).toByte)))
}
