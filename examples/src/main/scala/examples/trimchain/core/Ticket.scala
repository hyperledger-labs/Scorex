package examples.trimchain.core

import com.google.common.primitives.{Bytes, Shorts}
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

  override def toString: String = json.noSpaces
}

object TicketSerializer extends Serializer[Ticket] {

  val MinerKeySize = Curve25519.KeyLength

  override def toBytes(obj: Ticket): Array[Byte] = {
    val proofsBytes = obj.partialProofs.map { bytes =>
      require(bytes.length == bytes.length.toShort)
      Shorts.toByteArray(bytes.length.toShort) ++ bytes
    }
    Bytes.concat(obj.minerKey, Shorts.toByteArray(obj.partialProofs.length.toShort),
      scorex.core.utils.concatBytes(proofsBytes))
  }


  override def parseBytes(bytes: Array[Byte]): Try[Ticket] = Try {
    @tailrec
    def parseProofs(index: Int, proofNum: Int, acc: Seq[Array[Byte]] = Seq.empty): Seq[Array[Byte]] = {
      if (proofNum > 0) {
        val proofSize = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val proof = bytes.slice(index + 2, index + 2 + proofSize)
        parseProofs(index + 2 + proofSize, proofNum - 1, proof +: acc)
      } else {
        acc
      }
    }

    val minerKey = bytes.slice(0, MinerKeySize)
    val proofNum = Shorts.fromByteArray(bytes.slice(MinerKeySize, MinerKeySize + 2))
    val proofs = parseProofs(MinerKeySize + 2, proofNum).reverse
    Ticket(minerKey, proofs)
  }
}