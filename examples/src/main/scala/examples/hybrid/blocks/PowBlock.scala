package examples.hybrid.blocks

import com.google.common.primitives.{Ints, Longs}
import examples.hybrid.mining.PowMiner._
import examples.hybrid.state.SimpleBoxTransaction
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.crypto.encode.Base58

import scala.util.Try

class PowBlockHeader(
                      val parentId: BlockId,
                      val prevPosId: BlockId,
                      val timestamp: Block.Timestamp,
                      val nonce: Long,
                      val brothersCount: Int,
                      val brothersHash: Array[Byte]) {

  import PowBlockHeader._

  lazy val headerBytes =
    parentId ++
      prevPosId ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(nonce) ++
      Ints.toByteArray(brothersCount) ++
      brothersHash

  lazy val headerValid =
    brothersCount >= 0 &&
      timestamp >= 0

  def correctWork(difficulty: BigInt) = correctWorkDone(id, difficulty)

  lazy val id = FastCryptographicHash(headerBytes)

  override lazy val toString = s"PowBlockHeader(id: ${Base58.encode(id)})" +
    s"(parentId: ${Base58.encode(parentId)}, posParentId: ${Base58.encode(prevPosId)}, time: $timestamp, " +
    s"nonce: $nonce)"
}

object PowBlockHeader {
  //two pointers and 2 long values, 64 bit each
  val PowHeaderSize = NodeViewModifier.ModifierIdSize * 2 + 8 * 2 + 4 + FastCryptographicHash.DigestSize

  def parse(bytes: Array[Byte]): Try[PowBlockHeader] = Try {
    require(bytes.length == PowHeaderSize)
    val parentId = bytes.slice(0, 32)
    val prevPosId = bytes.slice(32, 64)
    val timestamp = Longs.fromByteArray(bytes.slice(64, 72))
    val nonce = Longs.fromByteArray(bytes.slice(72, 80))
    val brothersCount = Ints.fromByteArray(bytes.slice(80, 84))
    val brothersHash = bytes.slice(84, 116)

    new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash)
  }

  def correctWorkDone(id: Array[Byte], difficulty: BigInt): Boolean = {
    val target = MaxTarget / difficulty
    BigInt(1, id) < target
  }
}

case class PowBlock(override val parentId: BlockId,
                    override val prevPosId: BlockId,
                    override val timestamp: Block.Timestamp,
                    override val nonce: Long,
                    override val brothersCount: Int,
                    override val brothersHash: Array[Byte],
                    brothers: Seq[PowBlockHeader])
  extends PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash)
    with HybridPersistentNodeViewModifier
    with Block[PublicKey25519Proposition, SimpleBoxTransaction] {

  override type M = PowBlock

  override lazy val companion = PowBlockCompanion

  override lazy val version: Version = 0: Byte

  // no transactions a PoW block carries on
  override lazy val transactions: Option[Seq[SimpleBoxTransaction]] = None

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId


  lazy val header = new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash)

  lazy val brotherBytes = companion.brotherBytes(brothers)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "posParentId" -> Base58.encode(prevPosId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "brothers" -> brothers.map(b => Base58.encode(b.id).asJson).asJson
  ).asJson

  override lazy val toString: String = s"PoWBlock(${json.noSpaces})"

}

object PowBlockCompanion extends NodeViewModifierCompanion[PowBlock] {

  def brotherBytes(brothers: Seq[PowBlockHeader]): Array[Byte] = brothers.foldLeft(Array[Byte]()) { case (ba, b) =>
    ba ++ b.headerBytes
  }

  override def bytes(modifier: PowBlock): Array[Byte] =
    modifier.headerBytes ++ modifier.brotherBytes

  override def parse(bytes: Array[Byte]): Try[PowBlock] = {
    val headerBytes = bytes.slice(0, PowBlockHeader.PowHeaderSize)
    PowBlockHeader.parse(headerBytes).flatMap { header =>
      Try {
        val (bs, _) = (0 until header.brothersCount).foldLeft((Seq[PowBlockHeader](), PowBlockHeader.PowHeaderSize)) {
          case ((brothers, position), _) =>
            val bBytes = bytes.slice(position, position + PowBlockHeader.PowHeaderSize)

            (brothers :+ PowBlockHeader.parse(bBytes).get,
              position + PowBlockHeader.PowHeaderSize)
        }
        PowBlock(
          header.parentId,
          header.prevPosId,
          header.timestamp,
          header.nonce,
          header.brothersCount,
          header.brothersHash,
          bs
        )
      }
    }
  }
}

object PowBlock {
  val ModifierTypeId = 3: Byte
}
