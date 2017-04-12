package examples.tailchain.core

import com.google.common.primitives.Shorts
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.{JsonSerializable, Serializer}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.annotation.tailrec
import scala.util.Try

case class Ticket(minerKey: Array[Byte], partialProofs: Seq[Array[Byte]]) extends JsonSerializable {
  override lazy val json: Json = Map(
    "minerKey" -> Base58.encode(minerKey).asJson,
    "proofs" -> partialProofs.map(Base58.encode).asJson
  ).asJson
}

object TicketSerializer extends Serializer[Ticket] {

  val MinerKeySize = Curve25519.KeyLength

  override def toBytes(obj: Ticket): Array[Byte] = {
    val proofsBytes = obj.partialProofs.map { bytes =>
      require(bytes.length == bytes.length.toShort)
      Shorts.toByteArray(bytes.length.toShort) ++ bytes
    }
    obj.minerKey ++ proofsBytes.reduce(_ ++ _)
  }


  override def parseBytes(bytes: Array[Byte]): Try[Ticket] = Try {
    @tailrec
    def parseProofs(index: Int, acc: Seq[Array[Byte]] = Seq.empty): Seq[Array[Byte]] = {
      if (bytes.length > index) {
        val proofSize = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val proof = bytes.slice(index + 2, index + 2 + proofSize)
        parseProofs(index + 2 + proofSize, proof +: acc)
      } else {
        acc
      }
    }

    val minerKey = bytes.slice(0, MinerKeySize)
    val proofs = parseProofs(MinerKeySize).reverse
    Ticket(minerKey, proofs)
  }
}