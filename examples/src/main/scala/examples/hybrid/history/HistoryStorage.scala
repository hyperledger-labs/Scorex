package examples.hybrid.history

import java.math.BigInteger

import examples.hybrid.blocks._
import examples.hybrid.mining.{MiningConstants, PosForger}
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, Serializer}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier._
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging

import scala.util.Failure

class HistoryStorage(blocksStorage: LSMStore,
                     metaDb: DB,
                     settings: MiningConstants) extends ScorexLogging {

  // map from block id to difficulty at this state
  lazy val blockDifficulties = metaDb.hashMap("powDiff", Serializer.BYTE_ARRAY, Serializer.BIG_INTEGER).createOrOpen()

  //blockId -> score correspondence, for now score == height; that's not very secure,
  //see http://bitcoin.stackexchange.com/questions/29742/strongest-vs-longest-chain-and-orphaned-blocks
  private lazy val blockHeights = metaDb.hashMap("hidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  private lazy val orphanCountVar = metaDb.atomicLong("orphans", 0L).createOrOpen()

  private lazy val bestPowIdVar = metaDb.atomicVar("lastPow", Serializer.BYTE_ARRAY).createOrOpen()

  private lazy val bestPosIdVar = metaDb.atomicVar("lastPos", Serializer.BYTE_ARRAY).createOrOpen()

  //for now score = chain length; that's not very secure, see link above
  def height: Long = Math.max(heightOf(bestPowId).getOrElse(0L), heightOf(bestPosId).getOrElse(0L))

  def bestChainScore: Long = height

  def bestPowId: Array[Byte] = Option(bestPowIdVar.get()).getOrElse(settings.GenesisParentId)

  def bestPosId: Array[Byte] = Option(bestPosIdVar.get()).getOrElse(settings.GenesisParentId)

  def bestPowBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPowId).get.asInstanceOf[PowBlock]
  }

  def bestPosBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPosId).get.asInstanceOf[PosBlock]
  }

  def modifierById(blockId: ModifierId): Option[HybridBlock with
    Block[PublicKey25519Proposition, SimpleBoxTransaction]] = {
    blocksStorage.get(ByteArrayWrapper(blockId)).flatMap { bw =>
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

  def update(b: HybridBlock, diff: Option[(BigInt, Long)], isBest: Boolean) = {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    writeBlock(b)
    blockHeights.put(b.id, parentHeight(b) + 1)
    diff.foreach(d => setDifficulties(b.id, d._1, d._2))
    if (isBest) setBestBlock(b)
    metaDb.commit()
  }

  def setBestBlock(b: HybridBlock): Unit = b match {
    case powBlock: PowBlock =>
      bestPowIdVar.set(powBlock.id)
      bestPosIdVar.set(powBlock.prevPosId)
    case posBlock: PosBlock =>
      bestPosIdVar.set(posBlock.id)
      bestPowIdVar.set(posBlock.parentId)
  }

  private def writeBlock(b: HybridBlock) = {
    val typeByte = b match {
      case _: PowBlock =>
        PowBlock.ModifierTypeId
      case _: PosBlock =>
        PosBlock.ModifierTypeId
    }

    blocksStorage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
  }.ensuring(blocksStorage.get(ByteArrayWrapper(b.id)).isDefined)

  def setDifficulties(id: NodeViewModifier.ModifierId, powDiff: BigInt, posDiff: Long): Unit = {
    blockDifficulties.put(1.toByte +: id, powDiff.bigInteger)
    blockDifficulties.put(0.toByte +: id, BigInt(posDiff).bigInteger)
  }

  def getPoWDifficulty(idOpt: Option[NodeViewModifier.ModifierId]): BigInt = {
    idOpt match {
      case Some(id) if id sameElements settings.GenesisParentId =>
        settings.Difficulty
      case Some(id) =>
        BigInt(blockDifficulties.get(1.toByte +: id): BigInteger)
      case None if height > 0 =>
        BigInt(blockDifficulties.get(1.toByte +: bestPosId): BigInteger)
      case _ =>
        settings.Difficulty
    }
  }

  def getPoSDifficulty(id: NodeViewModifier.ModifierId): Long = if (id sameElements settings.GenesisParentId) {
    PosForger.InitialDifficuly
  } else {
    BigInt(blockDifficulties.get(0.toByte +: id): BigInteger).toLong
  }


  def parentHeight(b: HybridBlock): Long = if (isGenesis(b)) 0L else blockHeights.get(parentId(b))

  def parentId(block: HybridBlock): ModifierId = block match {
    case powBlock: PowBlock => powBlock.prevPosId
    case posBlock: PosBlock => posBlock.parentId
  }

  def heightOf(blockId: ModifierId): Option[Long] = Option(blockHeights.get(blockId)).map(_.toLong)

  def isGenesis(b: HybridBlock): Boolean = b.parentId sameElements settings.GenesisParentId

}
