package examples.trimchain.modifiers

import com.google.common.primitives.Longs
import examples.trimchain.core.Constants._
import examples.trimchain.core._
import io.circe.Json
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.utils.ByteBoxer
import scorex.crypto.encode.Base58
import supertagged.tag
import scala.util.Try

//TODO compact proof of ticket in header
case class BlockHeader(override val parentId: ByteBoxer[ModifierId],
                       stateRoot: StateRoot,
                       txRoot: TransactionsRoot,
                       ticket: Ticket,
                       powNonce: Long
                      ) extends TModifier {
  override type M = BlockHeader

  override val modifierTypeId: ModifierTypeId = TModifier.Header

  override lazy val id: ModifierId = ModifierId @@ Constants.hashfn.hash(bytes)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId.arr).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "txRoot" -> Base58.encode(txRoot).asJson,
    "ticket" -> ticket.json,
    "powNonce" -> powNonce.asJson
  ).asJson

  def correctWorkDone(difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

  override lazy val serializer = BlockHeaderSerializer
}

object BlockHeaderSerializer extends Serializer[BlockHeader] {
  private val ds = Constants.hashfn.DigestSize

  override def toBytes(obj: BlockHeader): Array[Byte] = obj.parentId.arr ++ obj.stateRoot ++ obj.txRoot ++
    Longs.toByteArray(obj.powNonce) ++ TicketSerializer.toBytes(obj.ticket)


  override def parseBytes(bytes: Array[Byte]): Try[BlockHeader] = Try {
    val parentId = ModifierId @@ bytes.slice(0, ds)
    val stateRoot = StateRoot @@ bytes.slice(ds, 2 * ds)
    val txRoot = TransactionsRoot @@ bytes.slice(2 * ds, 3 * ds)
    val powNonce = Longs.fromByteArray(bytes.slice(3 * ds, 3 * ds + 8))
    val ticket = TicketSerializer.parseBytes(bytes.slice(3 * ds + 8, bytes.length)).get
    BlockHeader(ByteBoxer[ModifierId](tag[ModifierId](parentId)), stateRoot, txRoot, ticket, powNonce)
  }
}
