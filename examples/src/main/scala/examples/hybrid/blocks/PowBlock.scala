package examples.hybrid.blocks

import com.google.common.primitives.{Ints, Longs}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.mining.HybridMiningSettings
import io.circe.Json
import io.circe.syntax._
import scorex.core._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey}

import scala.util.Try

class PowBlockHeader(
                      val parentId: BlockId,
                      val prevPosId: BlockId,
                      val timestamp: Block.Timestamp,
                      val nonce: Long,
                      val brothersCount: Int,
                      val brothersHash: Array[Byte],
                      val generatorProposition: PublicKey25519Proposition) {


  import PowBlockHeader._

  lazy val headerBytes =
    parentId ++
      prevPosId ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(nonce) ++
      Ints.toByteArray(brothersCount) ++
      brothersHash ++
      generatorProposition.pubKeyBytes

  def correctWork(difficulty: BigInt, s: HybridMiningSettings): Boolean = correctWorkDone(id, difficulty, s)

  lazy val id = ModifierId @@ Blake2b256(headerBytes)

  override lazy val toString = s"PowBlockHeader(id: ${Base58.encode(id)})" +
    s"(parentId: ${Base58.encode(parentId)}, posParentId: ${Base58.encode(prevPosId)}, time: $timestamp, " +
    s"nonce: $nonce)"
}

object PowBlockHeader {
  //two pointers and 2 long values, 64 bit each
  val PowHeaderSize = NodeViewModifier.ModifierIdSize * 2 + 8 * 2 + 4 + Blake2b256.DigestSize + Curve25519.KeyLength

  def parse(bytes: Array[Byte]): Try[PowBlockHeader] = Try {
    require(bytes.length == PowHeaderSize)
    val parentId = ModifierId @@ bytes.slice(0, 32)
    val prevPosId = ModifierId @@ bytes.slice(32, 64)
    val timestamp = Longs.fromByteArray(bytes.slice(64, 72))
    val nonce = Longs.fromByteArray(bytes.slice(72, 80))
    val brothersCount = Ints.fromByteArray(bytes.slice(80, 84))
    val brothersHash = bytes.slice(84, 116)
    val prop = PublicKey25519Proposition(PublicKey @@ bytes.slice(116, 148))

    new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, prop)
  }

  def correctWorkDone(id: Array[Byte], difficulty: BigInt, s: HybridMiningSettings): Boolean = {
    val target = s.MaxTarget / difficulty
    BigInt(1, id) < target
  }
}

case class PowBlock(override val parentId: BlockId,
                    override val prevPosId: BlockId,
                    override val timestamp: Block.Timestamp,
                    override val nonce: Long,
                    override val brothersCount: Int,
                    override val brothersHash: Array[Byte],
                    override val generatorProposition: PublicKey25519Proposition,
                    brothers: Seq[PowBlockHeader])
  extends PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, generatorProposition)
    with HybridBlock {

  override type M = PowBlock

  override lazy val serializer = PowBlockCompanion

  override lazy val version: Version = 0: Byte

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId


  lazy val header = new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, generatorProposition)

  lazy val brotherBytes = serializer.brotherBytes(brothers)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "prevPosId" -> Base58.encode(prevPosId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "brothersHash" -> Base58.encode(brothersHash).asJson,
    "brothers" -> brothers.map(b => Base58.encode(b.id).asJson).asJson
  ).asJson

  override lazy val toString: String = s"PoWBlock(${json.noSpaces})"

  //todo: coinbase transaction?
  override def transactions: Seq[SimpleBoxTransaction] = Seq()
}

object PowBlockCompanion extends Serializer[PowBlock] {

  def brotherBytes(brothers: Seq[PowBlockHeader]): Array[Byte] = brothers.foldLeft(Array[Byte]()) { case (ba, b) =>
    ba ++ b.headerBytes
  }

  override def toBytes(modifier: PowBlock): Array[Byte] =
    modifier.headerBytes ++ modifier.brotherBytes ++ modifier.generatorProposition.bytes

  override def parseBytes(bytes: Array[Byte]): Try[PowBlock] = {
    val headerBytes = bytes.slice(0, PowBlockHeader.PowHeaderSize)
    PowBlockHeader.parse(headerBytes).flatMap { header =>
      Try {
        val (bs, posit) = (0 until header.brothersCount).foldLeft((Seq[PowBlockHeader](), PowBlockHeader.PowHeaderSize)) {
          case ((brothers, position), _) =>
            val bBytes = bytes.slice(position, position + PowBlockHeader.PowHeaderSize)

            (brothers :+ PowBlockHeader.parse(bBytes).get,
              position + PowBlockHeader.PowHeaderSize)
        }
        val prop = PublicKey25519PropositionSerializer.parseBytes(bytes.slice(posit, posit + Curve25519.KeyLength)).get
        PowBlock(
          header.parentId,
          header.prevPosId,
          header.timestamp,
          header.nonce,
          header.brothersCount,
          header.brothersHash,
          prop,
          bs
        )
      }
    }
  }
}

object PowBlock {
  val ModifierTypeId: ModifierTypeId = scorex.core.ModifierTypeId @@ 3.toByte
}
