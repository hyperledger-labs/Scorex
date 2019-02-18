package examples.commons

import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.util.encode.Base16
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519
import scorex.util.serialization.{Reader, Writer}

case class PublicKey25519NoncedBox(override val proposition: PublicKey25519Proposition,
                                   override val nonce: Nonce,
                                   override val value: Value) extends PublicKeyNoncedBox[PublicKey25519Proposition] {

  override type M = PublicKey25519NoncedBox

  override def serializer: ScorexSerializer[PublicKey25519NoncedBox] = PublicKey25519NoncedBoxSerializer

  override def toString: String =
    s"PublicKey25519NoncedBox(id: ${Base16.encode(id)}, proposition: $proposition, nonce: $nonce, value: $value)"
}

object PublicKey25519NoncedBox extends ScorexEncoding {
  val BoxKeyLength: Int = Blake2b256.DigestSize
  val BoxLength: Int = Curve25519.KeyLength + 2 * 8

  implicit val publicKey25519NoncedBoxEncoder: Encoder[PublicKey25519NoncedBox] = (pknb: PublicKey25519NoncedBox) =>
    Map(
      "id" -> encoder.encode(pknb.id).asJson,
      "address" -> pknb.proposition.address.asJson,
      "publicKey" -> encoder.encode(pknb.proposition.pubKeyBytes).asJson,
      "nonce" -> pknb.nonce.toLong.asJson,
      "value" -> pknb.value.toLong.asJson
    ).asJson
}

object PublicKey25519NoncedBoxSerializer extends ScorexSerializer[PublicKey25519NoncedBox] {


  override def serialize(obj: PublicKey25519NoncedBox, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)
    w.putLong(obj.nonce)
    w.putULong(obj.value)
  }

  override def parse(r: Reader): PublicKey25519NoncedBox = {
    PublicKey25519NoncedBox(
      PublicKey25519PropositionSerializer.parse(r),
      Nonce @@ r.getLong(),
      Value @@ r.getULong()
    )
  }
}

