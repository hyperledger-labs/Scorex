package examples.hybrid.blocks

import com.google.common.primitives.Longs
import examples.hybrid.state.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import shapeless.{::, HNil}

import scala.util.Try

case class PosBlock(override val parentId: BlockId, //PoW block
                    override val timestamp: Block.Timestamp,
                    txs: Seq[SimpleBoxTransaction],
                    generator: PublicKey25519Proposition,
                    signature: Signature25519
                   ) extends HybridPersistentNodeViewModifier with
  Block[PublicKey25519Proposition, SimpleBoxTransaction] {
  override type M = PosBlock

  override type BlockFields = BlockId :: Timestamp :: Seq[SimpleBoxTransaction] :: PublicKey25519Proposition :: Signature25519 :: HNil

  override lazy val transactions: Some[Seq[SimpleBoxTransaction]] = Some(txs)

  override lazy val companion = PosBlockCompanion

  override lazy val version: Version = 0: Byte

  override lazy val blockFields = parentId :: timestamp :: txs :: generator :: signature :: HNil

  override lazy val modifierTypeId: ModifierTypeId = PosBlock.ModifierTypeId

  override lazy val id: ModifierId =
    FastCryptographicHash(parentId ++ Longs.toByteArray(timestamp) ++ generator.pubKeyBytes)

  override def json: Json = ???
}

object PosBlockCompanion extends NodeViewModifierCompanion[PosBlock] {
  //todo: for Dmitry
  override def bytes(block: PosBlock): Array[Version] = ???

  //todo: for Dmitry
  override def parse(bytes: Array[Version]): Try[PosBlock] = ???
}

object PosBlock {
  val MaxBlockSize = 65536 //64K
  val ModifierTypeId = 4: Byte
}
