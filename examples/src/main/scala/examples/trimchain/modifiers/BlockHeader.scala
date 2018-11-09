package examples.trimchain.modifiers

import examples.trimchain.core._
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.ModifierTypeId
import scorex.core.newserialization.{ScorexReader, ScorexSerializer, ScorexWriter}
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId, idToBytes}

//TODO compact proof of ticket in header
case class BlockHeader(override val parentId: ModifierId,
                       stateRoot: StateRoot,
                       txRoot: TransactionsRoot,
                       ticket: Ticket,
                       powNonce: Long
                      ) extends TModifier {
  override val modifierTypeId: ModifierTypeId = TModifier.Header

  override lazy val id: ModifierId = bytesToId(Constants.hashfn.hash(BlockHeaderSerializer.toBytes(this)))

  def correctWorkDone(difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, idToBytes(id)) < target
  }
}

object BlockHeader extends ScorexEncoding {
  implicit val blockHeaderEncoder: Encoder[BlockHeader] = (bh: BlockHeader) =>
    Map(
      "id" -> encoder.encodeId(bh.id).asJson,
      "parentId" -> encoder.encodeId(bh.parentId).asJson,
      "stateRoot" -> encoder.encode(bh.stateRoot).asJson,
      "txRoot" -> encoder.encode(bh.txRoot).asJson,
      "ticket" -> bh.ticket.asJson,
      "powNonce" -> bh.powNonce.asJson
    ).asJson
}

object BlockHeaderSerializer extends ScorexSerializer[BlockHeader] {
  private val ds = Constants.hashfn.DigestSize

  override def serialize(obj: BlockHeader, w: ScorexWriter): Unit = {
    w.putBytes(idToBytes(obj.parentId))
    w.putBytes(obj.stateRoot)
    w.putBytes(obj.txRoot)
    w.putLong(obj.powNonce)
    TicketSerializer.serialize(obj.ticket, w)
  }

  override def parse(r: ScorexReader): BlockHeader = {
    val parentId = bytesToId(r.getBytes(ds))
    val stateRoot = StateRoot @@ r.getBytes(ds)
    val txRoot = TransactionsRoot @@ r.getBytes(ds)
    val powNonce = r.getLong()
    val ticket = TicketSerializer.parse(r)
    BlockHeader(parentId, stateRoot, txRoot, ticket, powNonce)
  }
}
