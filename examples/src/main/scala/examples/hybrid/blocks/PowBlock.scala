package examples.hybrid.blocks

import examples.commons.SimpleBoxTransaction
import examples.hybrid.mining.HybridMiningSettings
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

class PowBlockHeader(
                      val parentId: BlockId,
                      val prevPosId: BlockId,
                      val timestamp: Block.Timestamp,
                      val nonce: Long,
                      val brothersCount: Int,
                      val brothersHash: Array[Byte],
                      val generatorProposition: PublicKey25519Proposition) extends ScorexEncoding {


  import PowBlockHeader._

  def correctWork(difficulty: BigInt, s: HybridMiningSettings): Boolean = correctWorkDone(id, difficulty, s)

  lazy val id: ModifierId = bytesToId(Blake2b256(PowBlockHeaderSerializer.toByteString(this).toArray))

  override lazy val toString: String = s"PowBlockHeader(id: ${encoder.encodeId(id)})" +
    s"(parentId: ${encoder.encodeId(parentId)}, posParentId: ${encoder.encodeId(prevPosId)}, time: $timestamp, " +
    s"nonce: $nonce)"
}

object PowBlockHeaderSerializer extends ScorexSerializer[PowBlockHeader] {

  override def serialize(h: PowBlockHeader, w: Writer): Unit = {
     w.putBytes(idToBytes(h.parentId))
     w.putBytes(idToBytes(h.prevPosId))
     w.putLong(h.timestamp)
     w.putLong(h.nonce)
     w.putInt(h.brothersCount)
     w.putBytes(h.brothersHash)
     w.putBytes(h.generatorProposition.pubKeyBytes)
  }

  override def parse(r: Reader): PowBlockHeader = {
    val parentId = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    val prevPosId = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    val timestamp = r.getLong()
    val nonce = r.getLong()
    val brothersCount = r.getInt()
    val brothersHash = r.getBytes(Blake2b256.DigestSize)
    val prop = PublicKey25519PropositionSerializer.parse(r)

    new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, prop)
  }
}

object PowBlockHeader {

  def correctWorkDone(id: ModifierId, difficulty: BigInt, s: HybridMiningSettings): Boolean = {
    val target = s.MaxTarget / difficulty
    BigInt(1, idToBytes(id)) < target
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

  override lazy val version: Version = 0: Byte

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId


  lazy val header = new PowBlockHeader(parentId, prevPosId, timestamp, nonce, brothersCount, brothersHash, generatorProposition)

  override lazy val toString: String = s"PoWBlock(${this.asJson.noSpaces})"

  //todo: coinbase transaction?
  override def transactions: Seq[SimpleBoxTransaction] = Seq()
}

object PowBlockSerializer extends ScorexSerializer[PowBlock] {

  def brotherBytes(brothers: Seq[PowBlockHeader]): Array[Byte] = {
    val w = new VLQByteStringWriter
    brothers.foreach(b => PowBlockHeaderSerializer.serialize(b ,w))
    w.result().toArray
  }

  override def serialize(block: PowBlock, w: Writer): Unit = {
    PowBlockHeaderSerializer.serialize(block.header, w)
    block.brothers.foreach(b => PowBlockHeaderSerializer.serialize(b ,w))
    PublicKey25519PropositionSerializer.serialize(block.generatorProposition, w)
  }

  override def parse(r: Reader): PowBlock = {
    val header = PowBlockHeaderSerializer.parse(r)
    val brothers = (0 until header.brothersCount).map{ _ =>
      PowBlockHeaderSerializer.parse(r)
    }
    val proposition = PublicKey25519PropositionSerializer.parse(r)
    PowBlock(
      header.parentId,
      header.prevPosId,
      header.timestamp,
      header.nonce,
      header.brothersCount,
      header.brothersHash,
      proposition,
      brothers
    )
  }
}

object PowBlock extends ScorexEncoding {
  val ModifierTypeId: ModifierTypeId = scorex.core.ModifierTypeId @@ 3.toByte

  implicit val powBlockEncoder: Encoder[PowBlock] = (pb: PowBlock) => {
    Map(
      "id" -> encoder.encodeId(pb.id).asJson,
      "parentId" -> encoder.encodeId(pb.parentId).asJson,
      "prevPosId" -> encoder.encodeId(pb.prevPosId).asJson,
      "timestamp" -> pb.timestamp.asJson,
      "nonce" -> pb.nonce.asJson,
      "brothersHash" -> encoder.encode(pb.brothersHash).asJson,
      "brothers" -> pb.brothers.map(b => encoder.encodeId(b.id).asJson).asJson
    ).asJson
  }
}
