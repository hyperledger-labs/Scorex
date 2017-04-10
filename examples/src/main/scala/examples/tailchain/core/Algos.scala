package examples.tailchain.core

import com.google.common.primitives.{Ints, Longs}
import examples.tailchain.modifiers.{BlockHeader, TBlock}
import examples.tailchain.utxo.AuthenticatedUtxo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

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
                     ctr: Long): Try[Ticket] = Try {
    require(utxos.size == k)

    var seed = hashfn(Longs.toByteArray(ctr))

    val partialProofs = utxos.zipWithIndex.map { case (utxo, idx) =>
      val id = hashfn(seed ++ minerPubKey ++ Ints.toByteArray(idx))
      val proof = utxo.lookupProof(id).get
      seed = id
      PartialProof(id, utxo.rootHash, proof)
    }

    Ticket(minerPubKey, ctr, partialProofs)
  }

  def pow(parentId: Array[Byte],
          transactionsRoot: Array[Byte],
          currentStateRoot: Array[Byte], //after applying transactions
          minerPubKey: Array[Byte],
          miningUtxos: IndexedSeq[AuthenticatedUtxo],
          difficulty: BigInt,
          attempts: Int): Option[BlockHeader] = {

    require(minerPubKey.length == Curve25519.KeyLength)
    require(miningUtxos.length = k)

    (1 to attempts).foreach { _ =>
      val st = hashfn(parentId, transactionsRoot, currentStateRoot)
      val nonce = Random.nextLong(Long.MaxValue)
      val ticket = generateTicket(miningUtxos, st, minerPubKey, nonce)
      val header = BlockHeader(parentId, currentStateRoot, transactionsRoot, ticket, nonce)

      if (header.correctWorkDone(difficulty)) return Some(header)
    }
    None
  }

  println(chooseSnapshots(9000, Array.fill(32)(Random.nextInt(100).toByte)))


}
