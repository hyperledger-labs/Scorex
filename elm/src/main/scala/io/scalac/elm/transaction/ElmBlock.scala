package io.scalac.elm.transaction

import io.circe._
import io.circe.generic.auto._
import io.scalac.elm.serialization.Serialization
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import shapeless.{::, HNil}


object ElmBlock extends Serialization[ElmBlock] {
  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long

  val codec = getCodec
}

case class ElmBlock(
  parentId: Block.BlockId,
  timestamp: Long,
  generationSignature: ElmBlock.GenerationSignature,
  baseTarget: ElmBlock.BaseTarget,
  generator: PublicKey25519Proposition,
  txs: Seq[ElmTransaction]
) extends Block[PublicKey25519Proposition, ElmTransaction] {

  import Block._
  import ElmBlock._

  override def transactions: Option[Seq[ElmTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[ElmBlock] = ElmBlock

  override def id: BlockId = FastCryptographicHash(ElmBlock.bytes(this))

  override type M = ElmBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[ElmTransaction] :: HNil

  override val version: Version = 0: Byte

  override def json: Json = ElmBlock.toJson(this)

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget :: generator :: txs :: HNil
}