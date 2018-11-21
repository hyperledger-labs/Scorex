package examples.trimchain.modifiers

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionSerializer}
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block.{Timestamp, Version}
import scorex.util.serialization.{Reader, Writer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId

import scala.annotation.tailrec

case class TBlock(header: BlockHeader, body: Seq[SimpleBoxTransaction], timestamp: Timestamp)
  extends TModifier with Block[SimpleBoxTransaction] {

  override def version: Version = 0: Version

  override def parentId: ModifierId = header.parentId

  override def transactions: Seq[SimpleBoxTransaction] = body

  override val modifierTypeId: ModifierTypeId = TModifier.Block

  override def id: ModifierId = header.id
}

object TBlockSerializer extends ScorexSerializer[TBlock] {


  override def serialize(obj: TBlock, w: Writer): Unit = {
    w.putLong(obj.timestamp)
    val headerWriter = w.newWriter()
    BlockHeaderSerializer.serialize(obj.header, headerWriter)
    w.putShort(headerWriter.length().toShort)
    w.append(headerWriter)

    obj.body.foreach { tx =>
      val txw = w.newWriter()
      SimpleBoxTransactionSerializer.serialize(tx, txw)
      w.putShort(txw.length().toShort)
      w.append(txw)
    }
  }

  override def parse(r: Reader): TBlock = {
    val timestamp = r.getLong()
    val headerLength = r.getShort()
    val header = BlockHeaderSerializer.parse(r.newReader(r.getChunk(headerLength)))

    @tailrec
    def parseTxs(acc: Seq[SimpleBoxTransaction] = Seq()): Seq[SimpleBoxTransaction] = {
      if (r.remaining > 0) {
        val txLength = r.getShort()
        val tx = SimpleBoxTransactionSerializer.parse(r.newReader(r.getChunk(txLength)))
        parseTxs(tx +: acc)
      } else {
        acc
      }
    }
    val body = parseTxs().reverse
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
