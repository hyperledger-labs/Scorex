package examples.commons

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.hybrid.wallet.HBoxWallet
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.newserialization._
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.transaction.proof.{Proof, Signature25519, Signature25519Serializer}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.util.ModifierId

import scala.util.Try

//A transaction orders to destroy boxes associated with (pubkey -> nonce) and create new boxes (pubkey -> nonce)
// where a nonce is derived from a transaction and also a box index

// WARNING!: the scheme is not provably secure to replay attacks etc
case class SimpleBoxTransaction(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                to: IndexedSeq[(PublicKey25519Proposition, Value)],
                                signatures: IndexedSeq[Signature25519],
                                override val fee: Long,
                                override val timestamp: Long) extends
  BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox] {


  override val messageToSign: Array[Byte] = {
    val newBoxesBytes = if (newBoxes.nonEmpty) {
      scorex.core.utils.concatBytes(newBoxes.map(PublicKey25519NoncedBoxSerializer.toBytes))
    } else {
      Array[Byte]()
    }
    Bytes.concat(
      newBoxesBytes,
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  }

  lazy val boxIdsToOpen: IndexedSeq[ADKey] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: ADKey = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces = Blake2b256(
    Bytes.concat(scorex.core.utils.concatFixLengthBytes(to.map(_._1.pubKeyBytes)),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  )

  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = SimpleBoxTransaction.nonceFromDigest(Blake2b256(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  override def toString: String = s"SimpleBoxTransaction(${this.asJson.noSpaces})"

  lazy val semanticValidity: Try[Unit] = Try {
    require(from.size == signatures.size)
    require(to.forall(_._2 >= 0))
    require(fee >= 0)
    require(timestamp >= 0)
    require(boxIdsToOpen.map(to => ByteArrayWrapper(to)).distinct.size == boxIdsToOpen.size)
    require(from.zip(signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, messageToSign)
    })
  }
}


object SimpleBoxTransaction extends ScorexEncoding {

  implicit val simpleBoxEncoder: Encoder[SimpleBoxTransaction] = (sbe: SimpleBoxTransaction) =>
    Map(
      "id" -> encoder.encodeId(sbe.id).asJson,
      "newBoxes" -> sbe.newBoxes.map(b => encoder.encode(b.id).asJson).toSeq.asJson,
      "boxesToRemove" -> sbe.boxIdsToOpen.map(id => encoder.encode(id).asJson).asJson,
      "from" -> sbe.from.map { s =>
        Map(
          "proposition" -> encoder.encode(s._1.pubKeyBytes).asJson,
          "nonce" -> s._2.toLong.asJson
        ).asJson
      }.asJson,
      "to" -> sbe.to.map { s =>
        Map(
          "proposition" -> encoder.encode(s._1.pubKeyBytes).asJson,
          "value" -> s._2.toLong.asJson
        ).asJson
      }.asJson,
      "signatures" -> sbe.signatures.map(s => encoder.encode(s.signature).asJson).asJson,
      "fee" -> sbe.fee.asJson,
      "timestamp" -> sbe.timestamp.asJson
    ).asJson

  def nonceFromDigest(digest: Array[Byte]): Nonce = Nonce @@ Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): SimpleBoxTransaction = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Signature @@ Array[Byte]()))

    val undersigned = SimpleBoxTransaction(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new SimpleBoxTransaction(fromPub, to, sigs, fee, timestamp)
  }

  def create(w: HBoxWallet,
             to: Seq[(PublicKey25519Proposition, Value)],
             fee: Long,
             boxesIdsToExclude: Seq[Array[Byte]] = Seq()): Try[SimpleBoxTransaction] = Try {
    var s = 0L
    val amount = to.map(_._2.toLong).sum

    val from: IndexedSeq[(PrivateKey25519, Nonce, Value)] = w.boxes()
      .filter(b => !boxesIdsToExclude.exists(id => java.util.Arrays.equals(id, b.box.id))).sortBy(_.createdAt).takeWhile { b =>
      s = s + b.box.value
      s < amount + b.box.value
    }.flatMap { b =>
      w.secretByPublicImage(b.box.proposition).map(s => (s, b.box.nonce, b.box.value))
    }.toIndexedSeq
    val canSend = from.map(_._3.toLong).sum
    // TODO: fixme, What should we do if `w.publicKeys` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val charge: (PublicKey25519Proposition, Value) = (w.publicKeys.head, Value @@ (canSend - amount - fee))

    val outputs: IndexedSeq[(PublicKey25519Proposition, Value)] = (to :+ charge).toIndexedSeq

    require(from.map(_._3.toLong).sum - outputs.map(_._2.toLong).sum == fee)

    val timestamp = System.currentTimeMillis()
    SimpleBoxTransaction(from.map(t => t._1 -> t._2), outputs, fee, timestamp)
  }
}

object SimpleBoxTransactionSerializer extends ScorexSerializer[SimpleBoxTransaction] {

  override def serialize(m: SimpleBoxTransaction, w: ScorexWriter): Unit = {
    w.putLong(m.fee)
    w.putLong(m.timestamp)
    w.putInt(m.signatures.length)
    w.putInt(m.from.length)
    w.putInt(m.to.length)
    m.signatures.foreach( s =>
      Signature25519Serializer.serialize(s, w)
    )
    m.from.foreach { f =>
      PublicKey25519PropositionSerializer.serialize(f._1, w)
      w.putLong(f._2)
    }

    m.to.foreach { t =>
      PublicKey25519PropositionSerializer.serialize(t._1, w)
      w.putLong(t._2)
    }
  }

  override def parse(r: ScorexReader): SimpleBoxTransaction = {
    val fee = r.getLong()
    val timestamp = r.getLong()
    val sigLength = r.getInt()
    val fromLength = r.getInt()
    val toLength = r.getInt()
    val signatures = (0 until sigLength) map { i =>
      Signature25519Serializer.parse(r)
    }
    val from = (0 until fromLength) map { i =>
      val pk = PublicKey25519PropositionSerializer.parse(r)
      val v = r.getLong()
      (pk, Nonce @@ v)
    }
    val to = (0 until toLength) map { i =>
      val pk = PublicKey25519PropositionSerializer.parse(r)
      val v = r.getLong()
      (pk, Value @@ v)
    }
    SimpleBoxTransaction(from, to, signatures, fee, timestamp)
  }
}
