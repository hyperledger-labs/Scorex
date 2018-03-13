package examples.hybrid.history

import com.google.common.primitives.Longs
import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks._
import examples.hybrid.mining.{HybridMiningSettings, PosForger}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.ModifierId
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Sha256

import scala.util.Failure

class HistoryStorage(storage: LSMStore,
                     settings: HybridMiningSettings) extends ScorexLogging {

  private val bestPowIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))
  private val bestPosIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-2: Byte))

  def height: Long = Math.max(heightOf(bestPowId).getOrElse(0L), heightOf(bestPosId).getOrElse(0L))

  def bestChainScore: Long = height

  def bestPowId: ModifierId = storage.get(bestPowIdKey).map(d => ModifierId @@ d.data)
    .getOrElse(settings.GenesisParentId)

  def bestPosId: ModifierId = storage.get(bestPosIdKey).map(d => ModifierId @@ d.data)
    .getOrElse(settings.GenesisParentId)

  def bestPowBlock: PowBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPowId).get.asInstanceOf[PowBlock]
  }

  def bestPosBlock: PosBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPosId).get.asInstanceOf[PosBlock]
  }

  def modifierById(blockId: ModifierId): Option[HybridBlock with
    Block[PublicKey25519Proposition, SimpleBoxTransaction]] = {
    storage.get(ByteArrayWrapper(blockId)).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      val parsed = mtypeId match {
        case t: Byte if t == PowBlock.ModifierTypeId =>
          PowBlockCompanion.parseBytes(bytes.tail)
        case t: Byte if t == PosBlock.ModifierTypeId =>
          PosBlockCompanion.parseBytes(bytes.tail)
      }
      parsed match {
        case Failure(e) => log.warn("Failed to parse bytes from bd", e)
        case _ =>
      }
      parsed.toOption
    }
  }

  def update(b: HybridBlock, diff: Option[(BigInt, Long)], isBest: Boolean) {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    val typeByte = b match {
      case _: PowBlock =>
        PowBlock.ModifierTypeId
      case _: PosBlock =>
        PosBlock.ModifierTypeId
    }

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = diff.map { d =>
      Seq(blockDiffKey(b.id, isPos = false) -> ByteArrayWrapper(d._1.toByteArray),
        blockDiffKey(b.id, isPos = true) -> ByteArrayWrapper(Longs.toByteArray(d._2)))
    }.getOrElse(Seq())

    val bestBlockSeq: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b match {
      case powBlock: PowBlock if isBest =>
        Seq(bestPowIdKey -> ByteArrayWrapper(powBlock.id), bestPosIdKey -> ByteArrayWrapper(powBlock.prevPosId))
      case posBlock: PosBlock if isBest =>
        Seq(bestPowIdKey -> ByteArrayWrapper(posBlock.parentId.arr), bestPosIdKey -> ByteArrayWrapper(posBlock.id))
      case _ => Seq()
    }

    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      blockDiff ++ blockH ++ bestBlockSeq ++ Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
  }

  def getPoWDifficulty(idOpt: Option[ModifierId]): BigInt = {
    idOpt match {
      case Some(id) if id sameElements settings.GenesisParentId =>
        settings.initialDifficulty
      case Some(id) =>
        BigInt(storage.get(blockDiffKey(id, isPos = false)).get.data)
      case None if height > 0 =>
        BigInt(storage.get(blockDiffKey(bestPosId, isPos = false)).get.data)
      case _ =>
        settings.initialDifficulty
    }
  }

  def getPoSDifficulty(id: ModifierId): Long = if (id sameElements settings.GenesisParentId) {
    PosForger.InitialDifficuly
  } else {
    Longs.fromByteArray(storage.get(blockDiffKey(id, isPos = true)).get.data)
  }

  def parentHeight(b: HybridBlock): Long = heightOf(parentId(b)).getOrElse(0L)

  def parentId(block: HybridBlock): ModifierId = block match {
    case powBlock: PowBlock => powBlock.prevPosId
    case posBlock: PosBlock => ModifierId !@@ posBlock.parentId.arr
  }

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes ++ blockId))

  private def blockDiffKey(blockId: Array[Byte], isPos: Boolean): ByteArrayWrapper =
    ByteArrayWrapper(Sha256(s"difficulties$isPos".getBytes ++ blockId))

  def heightOf(blockId: ModifierId): Option[Long] = storage.get(blockHeightKey(blockId))
    .map(b => Longs.fromByteArray(b.data))

  def isGenesis(b: HybridBlock): Boolean = b match {
    case powB: PowBlock => powB.parentId.arr sameElements settings.GenesisParentId
    case posB: PosBlock => heightOf(ModifierId !@@ posB.parentId.arr).contains(1L)
  }
}