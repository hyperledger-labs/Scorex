package examples.trimchain.modifiers

import com.google.common.primitives.{Longs, Shorts}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionCompanion}
import io.circe.{Encoder, Json}
import io.circe.syntax._
import scorex.core.block.Block
import scorex.core.block.Block.{Timestamp, Version}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{ModifierId, ModifierTypeId}

import scala.annotation.tailrec
import scala.util.Try

case class TBlock(header: BlockHeader, body: Seq[SimpleBoxTransaction], timestamp: Timestamp)
  extends TModifier with Block[PublicKey25519Proposition, SimpleBoxTransaction] {

  override def version: Version = 0: Version

  override def parentId: ModifierId = header.parentId

  override def transactions: Seq[SimpleBoxTransaction] = body

  override val modifierTypeId: ModifierTypeId = TModifier.Block

  override def id: ModifierId = header.id

  override type M = TBlock

  override def serializer: Serializer[TBlock] = TBlockSerializer
}

object TBlockSerializer extends Serializer[TBlock] {
  override def toBytes(obj: TBlock): Array[Byte] = {
    val txBytes: Array[Byte] = if (obj.body.isEmpty) {
      Array()
    } else {
      scorex.core.utils.concatBytes(obj.body.map { tx =>
        val transactionBytes = SimpleBoxTransactionCompanion.toBytes(tx)
        Shorts.toByteArray(transactionBytes.length.toShort) ++ transactionBytes
      })
    }
    val headerBytes = BlockHeaderSerializer.toBytes(obj.header)
    Longs.toByteArray(obj.timestamp) ++ Shorts.toByteArray(headerBytes.length.toShort) ++ headerBytes ++ txBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[TBlock] = Try {
    @tailrec
    def parseTxs(index: Int, acc: Seq[SimpleBoxTransaction] = Seq()): Seq[SimpleBoxTransaction] = {
      if (bytes.length > index) {
        val txLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val tx = SimpleBoxTransactionCompanion.parseBytes(bytes.slice(index + 2, index + 2 + txLength)).get
        parseTxs(index + 2 + txLength, tx +: acc)
      } else {
        acc
      }
    }
    val timestamp = Longs.fromByteArray(bytes.slice(0, 8))
    val headerLength = Shorts.fromByteArray(bytes.slice(8, 10))
    val header = BlockHeaderSerializer.parseBytes(bytes.slice(10, 10 + headerLength)).get
    val body = parseTxs(10 + headerLength).reverse
    TBlock(header, body, timestamp)
  }
}

object TBlock {
  implicit val tBlockEncoder: Encoder[TBlock] = (tb: TBlock) =>
    Map(
      "header" -> tb.header.asJson,
      "body" -> tb.body.map(_.asJson).asJson,
      "timestamp" -> tb.timestamp.asJson
    ).asJson
}