package examples.commons

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.hybrid.wallet.HBoxWallet
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.util.serialization.{VLQByteBufferWriter, _}
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.transaction.proof.{Proof, Signature25519, Signature25519Serializer}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Signature
import scorex.util.ByteArrayBuilder
import scorex.util.Extensions._

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

  override type M = SimpleBoxTransaction

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    newBoxes.foreach(box => PublicKey25519NoncedBoxSerializer.serialize(box, writer))
    unlockers.foreach(unlocker => writer.putBytes(unlocker.closedBoxId))
    writer.putULong(timestamp)
    writer.putULong(fee)
    writer.toBytes
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

  lazy val hashNoNonces = Blake2b256 {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    to.foreach { case (prop, _) => writer.putBytes(prop.pubKeyBytes) }
    unlockers.foreach(unlocker => writer.putBytes(unlocker.closedBoxId))
    writer.putULong(timestamp)
    writer.putULong(fee)
    writer.toBytes
  }

  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = SimpleBoxTransaction.nonceFromDigest(Blake2b256(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  override lazy val serializer = SimpleBoxTransactionSerializer

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

  override def serialize(m: SimpleBoxTransaction, w: Writer): Unit = {
    w.putULong(m.fee)
    w.putULong(m.timestamp)
    w.putUInt(m.signatures.length)
    w.putUInt(m.from.length)
    w.putUInt(m.to.length)
    m.signatures.foreach( s =>
      Signature25519Serializer.serialize(s, w)
    )
    m.from.foreach { f =>
      PublicKey25519PropositionSerializer.serialize(f._1, w)
      w.putULong(f._2)
    }

    m.to.foreach { t =>
      PublicKey25519PropositionSerializer.serialize(t._1, w)
      w.putULong(t._2)
    }
  }

  override def parse(r: Reader): SimpleBoxTransaction = {
    val fee = r.getULong()
    val timestamp = r.getULong()
    val sigLength = r.getUInt()
    val fromLength = r.getUInt()
    val toLength = r.getUInt()
    val signatures = (0 until sigLength.toIntExact) map { i =>
      Signature25519Serializer.parse(r)
    }
    val from = (0 until fromLength.toIntExact) map { i =>
      val pk = PublicKey25519PropositionSerializer.parse(r)
      val v = r.getULong()
      (pk, Nonce @@ v)
    }
    val to = (0 until toLength.toIntExact) map { i =>
      val pk = PublicKey25519PropositionSerializer.parse(r)
      val v = r.getULong()
      (pk, Value @@ v)
    }
    SimpleBoxTransaction(from, to, signatures, fee, timestamp)
  }
}
