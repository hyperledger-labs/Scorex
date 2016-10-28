package examples.hybrid.blocks

import com.google.common.primitives.Longs
import examples.hybrid.mining.PowMiner
import examples.hybrid.state.SimpleBoxTransaction
import shapeless.{::, HNil}
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try


case class PowBlock(override val parentId: BlockId,
                    prevPosId: BlockId,
                    override val timestamp: Block.Timestamp,
                    nonce: Long)
  extends HybridPersistentNodeViewModifier with
    Block[PublicKey25519Proposition, SimpleBoxTransaction] {

  override type M = PowBlock

  override lazy val companion = PowBlockCompanion

  override type BlockFields = BlockId :: BlockId :: Block.Timestamp :: Long :: HNil

  override lazy val version: Version = 0: Byte

  // no transactions a PoW block carries on
  override lazy val transactions: Option[Seq[SimpleBoxTransaction]] = None

  override lazy val modifierTypeId: ModifierTypeId = PowBlock.ModifierTypeId

  override lazy val id: ModifierId = FastCryptographicHash(bytes)

  override lazy val blockFields: BlockFields = parentId :: prevPosId :: timestamp :: nonce :: HNil

  override lazy val json: Json = ???

  lazy val correctWork = BigInt(1, id) < PowMiner.Difficulty
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
}

object PowBlock {
  val ModifierTypeId = 3: Byte
}
