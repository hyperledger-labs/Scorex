package examples.trimchain.core

import io.circe.Encoder
import io.circe.syntax._
import scorex.core.newserialization.{ScorexReader, ScorexSerializer, ScorexWriter}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.signatures.Curve25519

import scala.annotation.tailrec

case class Ticket(minerKey: Array[Byte], partialProofs: Seq[SerializedAdProof]) {

  override def toString: String = this.asJson.noSpaces
}

object Ticket extends ScorexEncoding {
  implicit val ticketEncoder: Encoder[Ticket] = (t: Ticket) =>
    Map(
      "minerKey" -> encoder.encode(t.minerKey).asJson,
      "proofs" -> t.partialProofs.map(encoder.encode).asJson
    ).asJson
}

object TicketSerializer extends ScorexSerializer[Ticket] {

  val MinerKeySize: Int = Curve25519.KeyLength

  override def serialize(obj: Ticket, w: ScorexWriter): Unit = {
    w.putBytes(obj.minerKey)
    w.putShort(obj.partialProofs.length.toShort)

    obj.partialProofs.map { bytes =>
      require(bytes.length == bytes.length.toShort)
      w.putShort(bytes.length.toShort)
      w.putBytes(bytes)
    }
  }

  override def parse(r: ScorexReader): Ticket = {
    val minerKey = r.getBytes(MinerKeySize)
    val proofNum = r.getShort()

    @tailrec
    def parseProofs(proofNum: Int, acc: Seq[SerializedAdProof] = Seq.empty): Seq[SerializedAdProof] = {
      if (proofNum > 0) {
        val proofSize = r.getShort()
        val proof = SerializedAdProof @@ r.getBytes(proofSize)
        parseProofs(proofNum - 1, proof +: acc)
      } else {
        acc
      }
    }

    val proofs = parseProofs(proofNum).reverse
    Ticket(minerKey, proofs)
  }
}