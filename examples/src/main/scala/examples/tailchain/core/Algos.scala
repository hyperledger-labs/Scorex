package examples.tailchain.core

import java.io.File

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.tailchain.modifiers.BlockHeader
import examples.tailchain.utxo.AuthenticatedUtxo
import io.iohk.iodb.LSMStore
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.StateChanges
import scorex.crypto.signatures.Curve25519

import scala.util.{Random, Success, Try}


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

    Ticket(minerPubKey, partialProofs)
  }

  def pow(parentId: Array[Byte],
          transactionsRoot: Array[Byte],
          currentStateRoot: Array[Byte], //after applying transactions
          minerPubKey: Array[Byte],
          miningUtxos: IndexedSeq[AuthenticatedUtxo],
          difficulty: BigInt,
          attempts: Int): Try[Option[BlockHeader]] = Try {

    require(minerPubKey.length == Curve25519.KeyLength)
    require(miningUtxos.length == k)

    (1 to attempts).foreach { _ =>
      val st = hashfn(parentId ++ transactionsRoot ++ currentStateRoot)
      val nonce = Random.nextLong()
      val ticket = generateTicket(miningUtxos, st, minerPubKey, nonce).get
      val header = BlockHeader(parentId, currentStateRoot, transactionsRoot, ticket, nonce)

      if (header.correctWorkDone(difficulty)) return Success(Some(header))
    }
    None
  }

  new File("/tmp/utxo").mkdirs()
  val store = new LSMStore(new File("/tmp/utxo"))
  val u1 = AuthenticatedUtxo(store, None, Array.fill(32)(0: Byte))

  val pk1 = PublicKey25519Proposition(Array.fill(32)(Random.nextInt(100).toByte))
  val b1 = PublicKey25519NoncedBox(pk1, 1L, 10)
  val b2 = PublicKey25519NoncedBox(pk1, 2L, 20)
  val u2 = u1.applyChanges(StateChanges(Set(), Set(b1, b2)), Array.fill(32)(Random.nextInt(100).toByte)).get


  println(pow(Array.fill(32)(0: Byte), Array.fill(32)(0: Byte), u2.rootHash, pk1.pubKeyBytes,
    IndexedSeq(u2), Constants.Difficulty, 1000000))

  println(chooseSnapshots(9000, Array.fill(32)(Random.nextInt(100).toByte)))
}
