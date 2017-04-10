package examples.tailchain.core

import com.google.common.primitives.{Longs, Shorts}
import scorex.core.serialization.Serializer
import scorex.crypto.signatures.Curve25519

import scala.annotation.tailrec
import scala.util.Try

case class Ticket(minerKey: Array[Byte], nonce: Long, partialProofs: Seq[PartialProof])

object Ticket extends Serializer[Ticket] {

  val MinerKeySize = Curve25519.KeyLength

  override def toBytes(obj: Ticket): Array[Byte] = {
    val proofsBytes = obj.partialProofs.map { p =>
      val bytes = PartialProof.toBytes(p)
      require(bytes.length == bytes.length.toShort)
      Shorts.toByteArray(bytes.length.toShort) ++ bytes
    }
    obj.minerKey ++ Longs.toByteArray(obj.nonce) ++ proofsBytes.reduce(_ ++ _)
  }


  override def parseBytes(bytes: Array[Byte]): Try[Ticket] = Try {
    @tailrec
    def parseProofs(index: Int, acc: Seq[PartialProof] = Seq.empty): Seq[PartialProof] = {
      if (bytes.length > index) {
        val proofSize = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val proof = PartialProof.parseBytes(bytes.slice(index + 2, index + 2 + proofSize)).get
        parseProofs(index + 2 + proofSize, proof +: acc)
      } else {
        acc
      }
    }

    val minerKey = bytes.slice(0, MinerKeySize)
    val nonce = Longs.fromByteArray(bytes.slice(MinerKeySize, MinerKeySize + 8))
    val proofs = parseProofs(MinerKeySize + 8).reverse
    Ticket(minerKey, nonce, proofs)
  }
}