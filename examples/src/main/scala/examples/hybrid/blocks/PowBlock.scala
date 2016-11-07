package examples.hybrid.blocks

import com.google.common.primitives.Longs
import examples.hybrid.mining.PowMiner._
import examples.hybrid.state.SimpleBoxTransaction
import shapeless.{::, HNil}
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.util.Try


case class PowBlock(override val parentId: BlockId,
                    prevPosId: BlockId,
                    override val timestamp: Block.Timestamp,
                    nonce: Long)
  extends HybridPersistentNodeViewModifier with
    Block[PublicKey25519Proposition, SimpleBoxTransaction] {

  override type M = PowBlock

  override type BlockFields = BlockId :: BlockId :: Block.Timestamp :: Long :: HNil

  override lazy val version: Version = 0: Byte

  // no transactions a PoW block carries on
  override lazy val transactions: Option[Seq[SimpleBoxTransaction]] = None

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId

  override lazy val id: ModifierId = ???

  override lazy val blockFields: BlockFields = parentId :: prevPosId :: timestamp :: nonce :: HNil

  override lazy val json: Json = ???

//  lazy val correctWork = companion.workDone(id)
  lazy val correctWork = ???

  override lazy val toString = s"PowBlock(id: ${Base58.encode(id)})" +
    s"(parentId: ${Base58.encode(parentId)}, posParentId: ${Base58.encode(prevPosId)}, time: $timestamp, nonce: $nonce)"
}

object PowBlockCompanion extends NodeViewModifierCompanion[PowBlock] {
  //two pointers and 2 long values, 64 bit each
  val PowBlockSize = NodeViewModifier.ModifierIdSize * 2 + 8 * 2

  override def bytes(modifier: PowBlock): Array[Version] =
    modifier.parentId ++
      modifier.prevPosId ++
      Longs.toByteArray(modifier.timestamp) ++
      Longs.toByteArray(modifier.nonce)

  override def parse(bytes: Array[Version]): Try[PowBlock] = Try {
    require(bytes.length == PowBlockSize)
    val parentId = bytes.slice(0, 32)
    val prevPosId = bytes.slice(32, 64)
    val timestamp = Longs.fromByteArray(bytes.slice(64, 72))
    val nonce = Longs.fromByteArray(bytes.slice(72, 80))
    PowBlock(parentId, prevPosId, timestamp, nonce)
  }

  def workDone(id: Array[Byte]): Boolean = {
    val target = MaxTarget / Difficulty
    BigInt(1, id) < target
  }
}

object PowBlock {
  val ModifierTypeId = 3: Byte
}
