package examples.hybrid.state

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.state.SimpleBoxTransaction._
import io.circe.Json
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.crypto.sign.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Proof, Signature25519}

//a transaction order to destroy boxes associated with (pubkey -> nonce) and create new boxes (pubkey -> nonce)
// where a nonce is derived from a transaction and also a box index

// WARNING!: the scheme is not provably secure to replay attacks etc
case class SimpleBoxTransaction(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                to: IndexedSeq[(PublicKey25519Proposition, Value)],
                                signatures: IndexedSeq[Signature25519],
                                override val fee: Long,
                                override val timestamp: Long) extends
  BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox] {

  override type M = SimpleBoxTransaction

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  //todo: for Dmitry - implement
  override lazy val json: Json = ???

  //stateless validation
  lazy val valid: Boolean = {
    from.size == signatures.size &&
      to.forall(_._2 >= 0) &&
      fee >= 0 &&
      timestamp >= 0 &&
      from.zip(signatures).forall { case ((prop, _), proof) =>
        proof.isValid(prop, messageToSign)
      }
  }
}


object SimpleBoxTransaction {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): SimpleBoxTransaction = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = SimpleBoxTransaction(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new SimpleBoxTransaction(fromPub, to, sigs, fee, timestamp)
  }
}

//todo: for Am - convert into test
object TxPlayground extends App {
  val priv1 = PrivateKey25519Companion.generateKeys(Array.fill(32)(0: Byte))
  val priv2 = PrivateKey25519Companion.generateKeys(Array.fill(32)(1: Byte))

  val from = IndexedSeq(priv1._1 -> 500L, priv2._1 -> 1000L)
  val to = IndexedSeq(priv2._2 -> 1500L)
  val fee = 500L
  val timestamp = System.currentTimeMillis()

  val tx = SimpleBoxTransaction(from, to, fee, timestamp)

  assert(tx.valid)

  val wrongSig = Array.fill(1)(0: Byte) ++ tx.signatures.head.signature.tail
  val wrongSigsSer = Seq(wrongSig) ++ tx.signatures.tail.map(_.signature)
  val wrongSigs = wrongSigsSer.map(bs => Signature25519(bs)).toIndexedSeq

  val fromPub = IndexedSeq(priv1._2 -> 500L, priv2._2 -> 1000L)
  val wrongTx = new SimpleBoxTransaction(fromPub, to, wrongSigs, fee, timestamp)

  assert(!wrongTx.valid)

  val wrongFromPub = IndexedSeq(priv1._2 -> 100L, priv2._2 -> 1000L)
  val wrongTx2 = new SimpleBoxTransaction(wrongFromPub, to, tx.signatures, fee, timestamp)

  assert(!wrongTx2.valid)
}