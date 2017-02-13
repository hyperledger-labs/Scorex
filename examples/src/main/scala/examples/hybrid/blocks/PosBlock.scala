package examples.hybrid.blocks

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.state.{SimpleBoxTransaction, SimpleBoxTransactionCompanion}
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.annotation.tailrec
import scala.util.Try

case class PosBlock(override val parentId: BlockId, //PoW block
                    override val timestamp: Block.Timestamp,
                    txs: Seq[SimpleBoxTransaction],
                    generatorBox: PublicKey25519NoncedBox,
                    attachment: Array[Byte],
                    signature: Signature25519
                   ) extends HybridBlock {
  override type M = PosBlock

  override lazy val transactions: Option[Seq[SimpleBoxTransaction]] = Some(txs)

  override lazy val serializer = PosBlockCompanion

  override lazy val version: Version = 0: Byte

  override lazy val modifierTypeId: ModifierTypeId = PosBlock.ModifierTypeId

  override lazy val id: ModifierId =
    FastCryptographicHash(parentId ++ Longs.toByteArray(timestamp) ++ generatorBox.id ++ attachment)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "attachment" -> Base58.encode(attachment).asJson,
    "timestamp" -> timestamp.asJson,
    "transactions" -> txs.map(_.json).asJson,
    "generatorBox" -> generatorBox.json,
    "signature" -> Base58.encode(signature.bytes).asJson
  ).asJson

  override def toString: String = s"PoSBlock(${json.noSpaces})"
}

object PosBlockCompanion extends Serializer[PosBlock] {
  override def toBytes(b: PosBlock): Array[Version] = {
    val txsBytes = b.txs.sortBy(t => Base58.encode(t.id)).foldLeft(Array[Byte]()) { (a, b) =>
      Bytes.concat(Ints.toByteArray(b.bytes.length), b.bytes, a)
    }
    Bytes.concat(b.parentId, Longs.toByteArray(b.timestamp), b.generatorBox.bytes, b.signature.bytes,
      Ints.toByteArray( b.txs.length), txsBytes, Ints.toByteArray(b.attachment.length), b.attachment)
  }

  override def parseBytes(bytes: Array[Version]): Try[PosBlock] = Try {
    require(bytes.length <= PosBlock.MaxBlockSize)

    val parentId = bytes.slice(0, BlockIdLength)
    var position = BlockIdLength
    val timestamp = Longs.fromByteArray(bytes.slice(position, position + 8))
    position = position + 8

    val boxBytes = bytes.slice(position, position + PublicKey25519NoncedBoxSerializer.PublicKey25519NoncedBoxLength)
    val box = PublicKey25519NoncedBoxSerializer.parseBytes(boxBytes).get
    position = position + PublicKey25519NoncedBoxSerializer.PublicKey25519NoncedBoxLength

    val signature = Signature25519(bytes.slice(position, position + Signature25519.SignatureSize))
    position = position + Signature25519.SignatureSize

    val txsLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position = position + 4
    val txs: Seq[SimpleBoxTransaction] = (0 until txsLength) map { _ =>
      val l = Ints.fromByteArray(bytes.slice(position, position + 4))
      val tx = SimpleBoxTransactionCompanion.parseBytes(bytes.slice(position + 4, position + 4 + l)).get
      position = position + 4 + l
      tx
    }

    val attachmentLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    val attachment = bytes.slice(position + 4, position + 4 + attachmentLength)
    PosBlock(parentId, timestamp, txs, box, attachment, signature)
  }
}

object PosBlock {
  val MaxBlockSize = 65535
  //64K
  val ModifierTypeId = 4: Byte

  def create(parentId: BlockId,
             timestamp: Block.Timestamp,
             txs: Seq[SimpleBoxTransaction],
             box: PublicKey25519NoncedBox,
             attachment: Array[Byte],
             privateKey: PrivateKey25519): PosBlock = {
    val unsigned = PosBlock(parentId, timestamp, txs, box, attachment, Signature25519(Array.empty))
    val signature = Curve25519.sign(privateKey.privKeyBytes, unsigned.bytes)
    unsigned.copy(signature = Signature25519(signature))
  }
}
