package examples.hybrid.history

import com.google.common.primitives.Longs
import examples.hybrid.blocks._
import examples.hybrid.mining.{HybridMiningSettings, PosForger}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.ModifierId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.consensus.ModifierSemanticValidity.{Absent, Unknown}
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Sha256

import scala.util.{Failure, Random, Try}

//TODO: why we are using IODB if there's no rollback?
class HistoryStorage(storage: LSMStore,
                     settings: HybridMiningSettings) extends ScorexLogging {

  private val bestPowIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))
  private val bestPosIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-2: Byte))

  def height: Long = Math.max(heightOf(bestPowId).getOrElse(0L), heightOf(bestPosId).getOrElse(0L))

  def bestChainScore: Long = height

  def bestPowId: ModifierId = storage.get(bestPowIdKey).map(d => ModifierId @@ new String(d.data))
    .getOrElse(settings.GenesisParentId)

  def bestPosId: ModifierId = storage.get(bestPosIdKey).map(d => ModifierId @@ new String(d.data))
    .getOrElse(settings.GenesisParentId)

  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def bestPowBlock: PowBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPowId).get.asInstanceOf[PowBlock]
  }

  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def bestPosBlock: PosBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPosId).get.asInstanceOf[PosBlock]
  }

  def modifierById(blockId: ModifierId): Option[HybridBlock] = {
    storage.get(ByteArrayWrapper(blockId.getBytes("UTF-8"))).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      val parsed: Try[HybridBlock] = mtypeId match {
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


  def semanticValidity(id: ModifierId): ModifierSemanticValidity = {
    modifierById(id).map { b =>
      storage
        .get(validityKey(b))
        .map(_.data.head)
        .map(ModifierSemanticValidity.restoreFromCode)
        .getOrElse(Unknown)
    }.getOrElse(Absent)
  }

  def updateValidity(b: HybridBlock, status: ModifierSemanticValidity): Unit = {
    val version = ByteArrayWrapper(Sha256(scala.util.Random.nextString(20).getBytes("UTF-8")))
    storage.update(version, Seq(), Seq(validityKey(b) -> ByteArrayWrapper(Array(status.code))))
  }

  def update(b: HybridBlock, difficulty: Option[(BigInt, BigInt)], isBest: Boolean): Unit = {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    val typeByte = b match {
      case _: PowBlock =>
        PowBlock.ModifierTypeId
      case _: PosBlock =>
        PosBlock.ModifierTypeId
    }

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = difficulty.map { d =>
      Seq(blockDiffKey(b.id, isPos = false) -> ByteArrayWrapper(d._1.toByteArray),
        blockDiffKey(b.id, isPos = true) -> ByteArrayWrapper(d._2.toByteArray))
    }.getOrElse(Seq())

    val bestBlockSeq: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b match {
      case powBlock: PowBlock if isBest =>
        Seq(bestPowIdKey -> ByteArrayWrapper(powBlock.id.getBytes("UTF-8")), bestPosIdKey -> ByteArrayWrapper(powBlock.prevPosId.getBytes("UTF-8")))
      case posBlock: PosBlock if isBest =>
        Seq(bestPowIdKey -> ByteArrayWrapper(posBlock.parentId.getBytes("UTF-8")), bestPosIdKey -> ByteArrayWrapper(posBlock.id.getBytes("UTF-8")))
      case _ => Seq()
    }

    storage.update(
      ByteArrayWrapper(Random.nextString(20).getBytes),
      Seq(),
      blockDiff ++
        blockH ++
        bestBlockSeq ++
        Seq(ByteArrayWrapper(b.id.getBytes("UTF-8")) -> ByteArrayWrapper(typeByte +: b.bytes)))
  }

  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
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

  // TODO: review me .get
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def getPoSDifficulty(id: ModifierId): BigInt = if (id sameElements settings.GenesisParentId) {
    PosForger.InitialDifficuly
  } else {
    BigInt(storage.get(blockDiffKey(id, isPos = true)).get.data)
  }

  def parentHeight(b: HybridBlock): Long = heightOf(parentId(b)).getOrElse(0L)

  def parentId(block: HybridBlock): ModifierId = block match {
    case powBlock: PowBlock => powBlock.prevPosId
    case posBlock: PosBlock => posBlock.parentId
  }

  private def validityKey(b: HybridBlock): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("validity".getBytes("UTF-8") ++ b.id.getBytes("UTF-8")))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes("UTF-8") ++ blockId.getBytes("UTF-8")))

  private def blockDiffKey(blockId: ModifierId, isPos: Boolean): ByteArrayWrapper =
    ByteArrayWrapper(Sha256(s"difficulties$isPos$blockId"))

  def heightOf(blockId: ModifierId): Option[Long] = storage.get(blockHeightKey(blockId))
    .map(b => Longs.fromByteArray(b.data))

  def isGenesis(b: HybridBlock): Boolean = b match {
    case powB: PowBlock => powB.parentId sameElements settings.GenesisParentId
    case posB: PosBlock => heightOf(posB.parentId).contains(1L)
  }
}
