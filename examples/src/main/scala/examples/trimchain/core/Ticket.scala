package examples.trimchain.core

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.annotation.tailrec
import scala.util.Try

case class Ticket(minerKey: Array[Byte], partialProofs: Seq[SerializedAdProof]) {

  override def toString: String = this.asJson.noSpaces
}

object Ticket {
  implicit val ticketEncoder: Encoder[Ticket] = (t: Ticket) =>
    Map(
      "minerKey" -> Base58.encode(t.minerKey).asJson,
      "proofs" -> t.partialProofs.map(Base58.encode).asJson
    ).asJson
}

object TicketSerializer extends Serializer[Ticket] {

  val MinerKeySize: Int = Curve25519.KeyLength

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
    def parseProofs(index: Int, proofNum: Int, acc: Seq[SerializedAdProof] = Seq.empty): Seq[SerializedAdProof] = {
      if (proofNum > 0) {
        val proofSize = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val proof = SerializedAdProof @@ bytes.slice(index + 2, index + 2 + proofSize)
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