package examples.spv

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.commons.SimpleBoxTransaction
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import Constants._


import scala.util.Try

case class Header(parentId: BlockId,
                  transactionsRoot: Array[Byte],
                  timestamp: Block.Timestamp,
                  nonce: Int) extends PersistentNodeViewModifier[PublicKey25519Proposition, SimpleBoxTransaction] {
  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = 100: Byte

  override lazy val id: ModifierId = hashfn(bytes)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson
  ).asJson

  override type M = Header

  override def serializer: Serializer[Header] = HeaderSerializer
}

object HeaderSerializer extends Serializer[Header] {
  override def toBytes(h: Header): Array[Byte] = {
    Bytes.concat(h.parentId, h.transactionsRoot, Longs.toByteArray(h.timestamp), Ints.toByteArray(h.nonce))
  }

  override def parseBytes(bytes: Array[Byte]): Try[Header] = Try {
    val parentId = bytes.slice(0, 32)
    val transactionsRoot = bytes.slice(32, 64)
    val timestamp = Longs.fromByteArray(bytes.slice(64, 72))
    val nonce = Ints.fromByteArray(bytes.slice(72, 76))
    Header(parentId, transactionsRoot, timestamp, nonce)
  }

}