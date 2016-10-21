package io.scalac.elm.transaction

import com.google.common.primitives.{Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import io.scalac.elm.transaction.ElmBlock._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.NodeViewModifierCompanion
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import shapeless.{::, HNil}

import scala.util.Try

case class ElmBlock(
  parentId: BlockId,
  timestamp: Long,
  generationSignature: GenerationSignature,
  baseTarget: BaseTarget,
  generator: PublicKey25519Proposition,
  txs: Seq[ElmTransaction]
) extends Block[PublicKey25519Proposition, ElmTransaction] {

  override def transactions: Option[Seq[ElmTransaction]] = Some(txs)

  override def companion = ElmBlock

  override def id: BlockId = FastCryptographicHash(companion.messageToSing(this))

  override type M = ElmBlock

  override type BlockFields = BlockId :: Timestamp :: Version ::
    GenerationSignature :: BaseTarget :: PublicKey25519Proposition :: Seq[ElmTransaction] :: HNil

  override val version: Version = 0: Byte

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generationSignature" -> Base58.encode(generationSignature).asJson,
    "baseTarget" -> baseTarget.asJson,
    "generator" -> Base58.encode(generator.pubKeyBytes).asJson,
    "txs" -> txs.map(_.json).asJson
  ).asJson

  override lazy val blockFields: BlockFields = parentId :: timestamp :: version :: generationSignature :: baseTarget :: generator :: txs :: HNil
}

object ElmBlock extends NodeViewModifierCompanion[ElmBlock] {
  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long

  def messageToSing(block: ElmBlock): Array[Byte] = {
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntBytes) { case (bytes, tx) => bytes ++ tx.bytes }
    }
  }

  override def bytes(block: ElmBlock): Array[Byte] = {
    //TODO use messageToSign
    block.parentId ++
      Longs.toByteArray(block.timestamp) ++
      Array(block.version) ++
      block.generationSignature ++
      Longs.toByteArray(block.baseTarget) ++
      block.generator.pubKeyBytes ++ {
      val cntBytes = Ints.toByteArray(block.txs.size)
      block.txs.foldLeft(cntBytes) { case (bytes, tx) => bytes ++ tx.bytes }
    }
  }

  override def parse(bytes: Array[ModifierTypeId]): Try[ElmBlock] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(Block.BlockIdLength, Block.BlockIdLength + 8))
    val version = bytes.slice(Block.BlockIdLength + 8, Block.BlockIdLength + 9).head
    val s0 = Block.BlockIdLength + 9
    val generationSignature = bytes.slice(s0, s0 + SignatureLength)
    val baseTarget = Longs.fromByteArray(bytes.slice(s0 + SignatureLength, s0 + SignatureLength + 8))
    val s1 = s0 + SignatureLength + 8
    val generator = PublicKey25519Proposition(bytes.slice(s1, s1 + 32))
    val cnt = Ints.fromByteArray(bytes.slice(s1 + 32, s1 + 36))
    val s2 = s1 + 36
    val txs = (0 until cnt) map { i =>
      val bt = bytes.slice(s2 + ElmTransaction.TransactionLength * i, s2 + ElmTransaction.TransactionLength * (i + 1))
      ElmTransaction.parse(bt).get
    }
    ElmBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)
  }
}