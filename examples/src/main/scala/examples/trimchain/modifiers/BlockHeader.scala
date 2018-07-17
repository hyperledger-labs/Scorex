package examples.trimchain.modifiers

import com.google.common.primitives.{Bytes, Longs}
import examples.trimchain.core._
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexEncoding
import scorex.core._

import scala.util.Try

//TODO compact proof of ticket in header
case class BlockHeader(override val parentId: ModifierId,
                       stateRoot: StateRoot,
                       txRoot: TransactionsRoot,
                       ticket: Ticket,
                       powNonce: Long
                      ) extends TModifier {
  override type M = BlockHeader

  override val modifierTypeId: ModifierTypeId = TModifier.Header

  override lazy val id: ModifierId = bytesToId(Constants.hashfn.hash(bytes))

  def correctWorkDone(difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, idToBytes(id)) < target
  }

  override lazy val serializer = BlockHeaderSerializer
}

object BlockHeader extends ScorexEncoding {
  implicit val blockHeaderEncoder: Encoder[BlockHeader] = (bh: BlockHeader) =>
    Map(
      "id" -> encoder.encode(bh.id).asJson,
      "parentId" -> encoder.encode(bh.parentId).asJson,
      "stateRoot" -> encoder.encode(bh.stateRoot).asJson,
      "txRoot" -> encoder.encode(bh.txRoot).asJson,
      "ticket" -> bh.ticket.asJson,
      "powNonce" -> bh.powNonce.asJson
    ).asJson
}

object BlockHeaderSerializer extends Serializer[BlockHeader] {
  private val ds = Constants.hashfn.DigestSize

  override def toBytes(obj: BlockHeader): Array[Byte] = Bytes.concat(idToBytes(obj.parentId), obj.stateRoot, obj.txRoot,
    Longs.toByteArray(obj.powNonce), TicketSerializer.toBytes(obj.ticket))


  override def parseBytes(bytes: Array[Byte]): Try[BlockHeader] = Try {
    val parentId = bytesToId(bytes.slice(0, ds))
    val stateRoot = StateRoot @@ bytes.slice(ds, 2 * ds)
    val txRoot = TransactionsRoot @@ bytes.slice(2 * ds, 3 * ds)
    val powNonce = Longs.fromByteArray(bytes.slice(3 * ds, 3 * ds + 8))
    val ticket = TicketSerializer.parseBytes(bytes.slice(3 * ds + 8, bytes.length)).get
    BlockHeader(parentId, stateRoot, txRoot, ticket, powNonce)
  }
}
