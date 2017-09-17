package examples.trimchain.core

import java.io.File

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.{Nonce, Value}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.curvepos.transaction.PublicKey25519NoncedBox._
import examples.trimchain.modifiers.BlockHeader
import examples.trimchain.utxo.{AuthenticatedUtxo, PersistentAuthenticatedUtxo}
import io.iohk.iodb.LSMStore
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion}
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Lookup}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.crypto.signatures.{Curve25519, PublicKey}

import scala.util.{Failure, Random, Success, Try}


object Algos extends App {

  import Constants._

  def chooseSnapshots(chainLength: Int, publicKey: Array[Byte]): Seq[Int] = {
    (1 to k).map { i =>
      val h = BigInt(1, hashfn(publicKey ++ Array(i.toByte))).mod(n).toInt + (chainLength - n)
      if (h >= 0) h else 0
    }.sorted
  }

  def generateTicket(utxos: IndexedSeq[AuthenticatedUtxo],
                     st: Array[Byte],
                     minerPubKey: Array[Byte],
                     ctr: Long): Try[Ticket] = Try {
    require(utxos.size == k)

    var seed = hashfn(Longs.toByteArray(ctr))

    val partialProofs = utxos.zipWithIndex.map { case (utxo, stateIndex) =>
      val ids = (0 until NElementsInProof) map (elementIndex => ADKey @@ hashfn(seed ++ minerPubKey ++
        Ints.toByteArray(stateIndex) ++ Ints.toByteArray(elementIndex)))
      val proof = utxo.lookupProof(ids).get
      seed = hashfn(scorex.core.utils.concatFixLengthBytes(ids)) //TODO do we need it?
      proof
    }

    Ticket(minerPubKey, partialProofs)
  }

  def pow(parentId: ModifierId,
          transactionsRoot: TransactionsRoot,
          currentStateRoot: StateRoot, //after applying transactions
          minerPubKey: PublicKey,
          miningUtxos: IndexedSeq[AuthenticatedUtxo],
          difficulty: BigInt,
          attempts: Int): Try[Option[BlockHeader]] = Try {

    require(minerPubKey.length == Curve25519.KeyLength)
    require(miningUtxos.length == k)

    (1 to attempts).foreach { i =>
      val st = hashfn(parentId ++ transactionsRoot ++ currentStateRoot)
      val nonce = Random.nextLong()
      val ticket = generateTicket(miningUtxos, st, minerPubKey, nonce).get
      val header = BlockHeader(parentId, currentStateRoot, transactionsRoot, ticket, nonce)

      if (header.correctWorkDone(difficulty)) {
        println(s"pow done in $i attempts")
        return Success(Some(header))
      }
    }
    None
  }

  def validatePow(header: BlockHeader,
                  miningStateRoots: IndexedSeq[Array[Byte]],
                  difficulty: BigInt): Boolean = Try {
    require(header.correctWorkDone(difficulty))
    require(miningStateRoots.length == k)

    val nonce = header.powNonce
    val minerKey = header.ticket.minerKey

    var seed = hashfn(Longs.toByteArray(nonce))

    //todo: is proof malleability possible?
    header.ticket.partialProofs.zip(miningStateRoots).zipWithIndex.foreach { case ((pp, sroot), stateIndex) =>
      val ids = (0 until NElementsInProof) map (elementIndex => hashfn(seed ++ minerKey ++
        Ints.toByteArray(stateIndex) ++ Ints.toByteArray(elementIndex)))

      val v = new BatchAVLVerifier[Digest32, Blake2b256Unsafe](ADDigest @@ sroot, pp, keyLength = BoxKeyLength,
        valueLengthOpt = Some(BoxLength))

      ids.foreach(id => v.performOneOperation(Lookup(ADKey @@ id)).get)
      seed = hashfn(scorex.core.utils.concatFixLengthBytes(ids)) //TODO do we need it?
    }

    true
  }.recoverWith {
    case e =>
      e.printStackTrace()
      Failure(e)
  }.getOrElse(false)

  new File("/tmp/utxo").delete()
  new File("/tmp/utxo").mkdirs()
  val store = new LSMStore(new File("/tmp/utxo"))
  val u1 = PersistentAuthenticatedUtxo(store, 0, None, VersionTag @@ Array.fill(32)(0: Byte))

  val pk1 = PublicKey25519Proposition(PublicKey @@ Array.fill(32)(Random.nextInt(100).toByte))
  val b1 = PublicKey25519NoncedBox(pk1, Nonce @@ 1L, Value @@ 10L)
  val b2 = PublicKey25519NoncedBox(pk1, Nonce @@ 2L, Value @@ 20L)
  val u2 = u1.applyChanges(BoxStateChanges(Seq(Insertion(b1), Insertion(b2))),
    VersionTag @@ Array.fill(32)(Random.nextInt(100).toByte)).get


  val headerOpt = pow(ModifierId @@ Array.fill(32)(0: Byte),
    TransactionsRoot @@ Array.fill(32)(0: Byte),
    StateRoot @@ u2.rootHash,
    pk1.pubKeyBytes,
    IndexedSeq(u2),
    Constants.Difficulty, 500).get

  println(headerOpt)

  headerOpt.foreach(h => println(validatePow(h, IndexedSeq(u2.rootHash), Constants.Difficulty)))

  println(chooseSnapshots(9000, Array.fill(32)(Random.nextInt(100).toByte)))
}
