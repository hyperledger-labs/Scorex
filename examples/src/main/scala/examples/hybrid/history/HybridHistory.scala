package examples.hybrid.history


import java.io.File
import java.math.BigInteger

import examples.hybrid.blocks._
import examples.hybrid.mining.{MiningConstants, MiningSettings, PosForger}
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.util.FileFunctions
import examples.hybrid.validation.BlockValidator
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, Modifications}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
//todo: add some versioned field to the class
class HybridHistory(blocksStorage: LSMStore,
                    metaDb: DB,
                    logDirOpt: Option[String],
                    settings: MiningConstants,
                    validator: BlockValidator)
  extends History[PublicKey25519Proposition,
    SimpleBoxTransaction,
    HybridPersistentNodeViewModifier,
    HybridSyncInfo,
    HybridHistory] with ScorexLogging {

  import HybridHistory._

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  // map from block id to difficulty at this state
  lazy val blockDifficulties = metaDb.hashMap("powDiff", Serializer.BYTE_ARRAY, Serializer.BIG_INTEGER).createOrOpen()

  //blockId -> score correspondence, for now score == height; that's not very secure,
  //see http://bitcoin.stackexchange.com/questions/29742/strongest-vs-longest-chain-and-orphaned-blocks
  lazy val blockHeights = metaDb.hashMap("hidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  //for now score = chain length; that's not very secure, see link above
  lazy val height = blockHeights.get(bestPowId)
  lazy val bestChainScore = height

  lazy val orphanCountVar = metaDb.atomicLong("orphans", 0L).createOrOpen()

  private lazy val bestPowIdVar = metaDb.atomicVar("lastPow", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPowId: Array[Byte] = Option(bestPowIdVar.get()).getOrElse(settings.GenesisParentId)

  private lazy val bestPosIdVar = metaDb.atomicVar("lastPos", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPosId: Array[Byte] = Option(bestPosIdVar.get()).getOrElse(settings.GenesisParentId)

  lazy val bestPowBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPowId).get.asInstanceOf[PowBlock]
  }

  lazy val bestPosBlock = {
    require(height > 0, "History is empty")
    modifierById(bestPosId).get.asInstanceOf[PosBlock]
  }

  lazy val pairCompleted: Boolean =
    (bestPowId sameElements settings.GenesisParentId, bestPosId sameElements settings.GenesisParentId) match {
      case (true, true) => true
      case (false, true) => false
      case (false, false) => bestPosBlock.parentId sameElements bestPowId
      case (true, false) => ??? //shouldn't be
    }

  /**
    * Return specified number of PoW blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return PoW blocks, in reverse order (starting from the most recent one)
    */
  //TODO a lot of crimes committed here: .get, .asInstanceOf
  def lastPowBlocks(count: Int): Seq[PowBlock] = if (isEmpty) {
    Seq()
  } else {
    @tailrec
    def loop(b: PowBlock, acc: Seq[PowBlock] = Seq()): Seq[PowBlock] = if (acc.length >= count) {
      acc
    } else {
      modifierById(b.parentId) match {
        case Some(parent: PowBlock) => loop(parent, b +: acc)
        case _ => b +: acc
      }
    }
    loop(bestPowBlock)
  }

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height <= 0

  override def modifierById(blockId: ModifierId): Option[HybridPersistentNodeViewModifier] = {
    blocksStorage.get(ByteArrayWrapper(blockId)).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      (mtypeId match {
        case t: Byte if t == PowBlock.ModifierTypeId =>
          PowBlockCompanion.parseBytes(bytes.tail)
        case t: Byte if t == PosBlock.ModifierTypeId =>
          PosBlockCompanion.parseBytes(bytes.tail)
      }).toOption
    }
  }

  override def contains(id: ModifierId): Boolean =
    if (id sameElements settings.GenesisParentId) true else modifierById(id).isDefined

  private def writeBlock(b: HybridPersistentNodeViewModifier) = {
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

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: HybridPersistentNodeViewModifier):
  Try[(HybridHistory, Modifications[HybridPersistentNodeViewModifier])] = Try {
    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    val res: (HybridHistory, Modifications[HybridPersistentNodeViewModifier]) = block match {
      case powBlock: PowBlock =>

        val currentScore: Long = if (!isGenesis(powBlock)) {
          validator.checkPowConsensusRules(powBlock, getPoWDifficulty(Some(powBlock.prevPosId))).get
          Math.max(blockHeights.get(powBlock.parentId), blockHeights.get(powBlock.prevPosId))
        } else {
          log.info("Genesis block: " + Base58.encode(powBlock.id))
          0L
        }

        //update block storage and the scores index
        val blockId = powBlock.id

        writeBlock(powBlock)

        val blockScore = currentScore + 1
        blockHeights.put(blockId, blockScore)

        val modifications: Modifications[HybridPersistentNodeViewModifier] = if (isGenesis(powBlock)) {
          //genesis block
          bestPowIdVar.set(blockId)
          Modifications(powBlock.parentId, Seq(), Seq(powBlock))
        } else {
          if (blockScore > currentScore) {
            //check for chain switching
            if (!(powBlock.parentId sameElements bestPowId)) {
              log.info(s"Porcessing fork at ${Base58.encode(powBlock.id)}")

              val (newSuffix, oldSuffix) = commonBlockThenSuffixes(powBlock)

              //decrement
              orphanCountVar.addAndGet(oldSuffix.size - newSuffix.size)
              logDirOpt.foreach { logDir =>
                val record = s"${oldSuffix.size}, $bestChainScore"
                FileFunctions.append(logDir + "/forkdepth.csv", record)
              }

              val rollbackPoint = newSuffix.head

              val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)
              val applyBlocks = newSuffix.tail.map(id => modifierById(id).get)

              bestPowIdVar.set(blockId)
              Modifications(rollbackPoint, throwBlocks, applyBlocks)
            } else {
              bestPowIdVar.set(blockId)
              Modifications(powBlock.parentId, Seq(), Seq(powBlock))
            }
          } else if (blockScore == bestChainScore &&
            (bestPowBlock.parentId sameElements powBlock.parentId) &&
            (bestPowBlock.parentId sameElements powBlock.parentId) &&
            (bestPowBlock.brothersCount < powBlock.brothersCount)
          ) {
            //handle younger brother - replace current best PoW block with a brother
            val replacedBlock = bestPowBlock
            bestPowIdVar.set(blockId)
            Modifications(powBlock.prevPosId, Seq(replacedBlock), Seq(powBlock))
          } else {
            orphanCountVar.incrementAndGet()
            Modifications(powBlock.parentId, Seq(), Seq(powBlock))
          }
        }
        logDirOpt.foreach { logDir =>
          val record = s"${orphanCountVar.get()}, $bestChainScore"
          FileFunctions.append(logDir + "/orphans.csv", record)
        }
        (new HybridHistory(blocksStorage, metaDb, logDirOpt, settings, validator), modifications)


      case posBlock: PosBlock =>
        validator.checkPoSConsensusRules(posBlock).get

        val powParent = posBlock.parentId

        val blockId = posBlock.id

        writeBlock(posBlock)
        val parentHeight: Long = if (!isGenesis(posBlock)) blockHeights.get(posBlock.parentId)
        else 0L

        blockHeights.put(posBlock.id, parentHeight + 1)

        if (powParent sameElements bestPowId) bestPosIdVar.set(blockId)

        setDifficultiesForNewBlock(posBlock)
        val mod: Modifications[HybridPersistentNodeViewModifier] =
          Modifications(posBlock.parentId, Seq(), Seq(posBlock))

        (new HybridHistory(blocksStorage, metaDb, logDirOpt, settings, validator), mod) //no rollback ever
    }
    metaDb.commit()
    log.info(s"History: block ${Base58.encode(block.id)} appended to chain with score $bestChainScore")
    res
  }

  private def setDifficultiesForNewBlock(posBlock: PosBlock): Unit = {
    val parentHeight = blockHeights.get(posBlock.parentId)
    if (parentHeight > 0 && parentHeight % DifficultyRecalcPeriod == 0) {
      //recalc difficulties
      val powBlocks = lastPowBlocks(DifficultyRecalcPeriod)
      assert(powBlocks.length == DifficultyRecalcPeriod)
      val realTime = powBlocks.last.timestamp - powBlocks.head.timestamp
      val brothersCount = powBlocks.map(_.brothersCount).sum
      val expectedTime = (DifficultyRecalcPeriod + brothersCount) * settings.BlockDelay
      val oldPowDifficulty = getPoWDifficulty(Some(powBlocks.last.prevPosId))

      val newPowDiff = (oldPowDifficulty * expectedTime / realTime).max(BigInt(1L))

      val oldPosDifficulty = getPoSDifficulty(powBlocks.last.prevPosId)
      val newPosDiff = oldPosDifficulty * DifficultyRecalcPeriod / ((DifficultyRecalcPeriod + brothersCount) * 8 / 10)
      log.info(s"PoW difficulty changed at ${Base58.encode(posBlock.id)}: old $oldPowDifficulty, new $newPowDiff. " +
        s" ${powBlocks.last.timestamp} - ${powBlocks.head.timestamp} | $brothersCount")
      log.info(s"PoS difficulty changed: old $oldPosDifficulty, new $newPosDiff")
      setDifficulties(posBlock.id, newPowDiff, newPosDiff)
    } else {
      //Same difficulty as in previous block
      val parentPoSId: ModifierId = modifierById(posBlock.parentId).get.asInstanceOf[PowBlock].prevPosId
      setDifficulties(posBlock.id, getPoWDifficulty(Some(parentPoSId)), getPoSDifficulty(parentPoSId))
    }
  }

  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) Seq(settings.GenesisParentId)
    else if (pairCompleted) Seq(bestPowId, bestPosId)
    else Seq(bestPowId)

  override def applicable(block: HybridPersistentNodeViewModifier): Boolean = {
    block match {
      case pwb: PowBlock =>
        contains(pwb.parentId) && contains(pwb.prevPosId)
      case psb: PosBlock =>
        contains(psb.parentId)
    }
  }

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)],
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    def inList(m: HybridPersistentNodeViewModifier): Boolean = idInList(m.id) || isGenesis(m)
    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 sameElements id)

    //Look without limit for case difference between nodes is bigger then size
    chainBack(bestPowBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(chain) =>
        log.warn("Found chain without ids form remote")
        None
      case _ => None
    }
  }

  override def syncInfo(answer: Boolean): HybridSyncInfo =
    HybridSyncInfo(
      answer,
      lastPowBlocks(HybridSyncInfo.MaxLastPowBlocks).map(_.id),
      bestPosId)

  @tailrec
  private def divergentSuffix(otherLastPowBlocks: Seq[ModifierId],
                              suffixFound: Seq[ModifierId] = Seq()): Seq[ModifierId] = {
    val head = otherLastPowBlocks.head
    val newSuffix = suffixFound :+ head
    modifierById(head) match {
      case Some(b) =>
        newSuffix
      case None => if (otherLastPowBlocks.length <= 1) {
        Seq()
      } else {
        divergentSuffix(otherLastPowBlocks.tail, newSuffix)
      }
    }
  }

  def heightOf(blockId: ModifierId): Option[Long] = Option(blockHeights.get(blockId))

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult.Value = {
    //todo: check PoW header correctness, return cheater status for that
    //todo: return cheater status in other cases, e.g. PoW id is a correct PoS id


    val dSuffix = divergentSuffix(other.lastPowBlockIds.reverse)

    dSuffix.length match {
      case 0 =>
        log.warn(s"CompareNonsense: ${other.lastPowBlockIds.toList.map(Base58.encode)} vs ${Base58.encode(bestPowId)}")
        HistoryComparisonResult.Nonsense
      case 1 =>
        if (dSuffix.head sameElements bestPowId) {
          if (other.lastPosBlockId sameElements bestPosId) {
            HistoryComparisonResult.Equal
          } else if (pairCompleted) {
            HistoryComparisonResult.Older
          } else {
            HistoryComparisonResult.Younger
          }
        } else HistoryComparisonResult.Younger
      case _ =>
        // +1 to include common block
        val localSuffixLength = height - heightOf(dSuffix.last).get + 1
        val otherSuffixLength = dSuffix.length

        if (localSuffixLength < otherSuffixLength)
          HistoryComparisonResult.Older
        else if (localSuffixLength == otherSuffixLength)
          HistoryComparisonResult.Equal
        else HistoryComparisonResult.Younger
    }

    /*
    if (other.bestPowBlockId sameElements PowMiner.GenesisParentId) {
      HistoryComparisonResult.Younger
    } else blockById(other.bestPowBlockId) match {
      case Some(pb: PowBlock) =>
        if (pb.id sameElements bestPowId) {
          val prevPosId = pb.prevPosId
          val otherNext = !(other.bestPosBlockId sameElements prevPosId)
          val selfNext = !(bestPosId sameElements prevPosId)

          (otherNext, selfNext) match {
            case (true, true) =>
              HistoryComparisonResult.Equal
            case (true, false) =>
              HistoryComparisonResult.Older
            case (false, true) =>
              HistoryComparisonResult.Younger
            case (false, false) =>
              HistoryComparisonResult.Equal
          }
        } else HistoryComparisonResult.Younger
      case None =>
        HistoryComparisonResult.Older
    }*/
  }

  private def setDifficulties(id: NodeViewModifier.ModifierId, powDiff: BigInt, posDiff: Long): Unit = {
    blockDifficulties.put(1.toByte +: id, powDiff.bigInteger)
    blockDifficulties.put(0.toByte +: id, BigInt(posDiff).bigInteger)
  }

  private def getPoWDifficulty(idOpt: Option[NodeViewModifier.ModifierId]): BigInt = {
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

  private def getPoSDifficulty(id: NodeViewModifier.ModifierId): Long = if (id sameElements settings.GenesisParentId) {
    PosForger.InitialDifficuly
  } else {
    BigInt(blockDifficulties.get(0.toByte +: id): BigInteger).toLong
  }

  def debugDifficulties(): Json = {
    val m: Map[String, String] = blockDifficulties.keySet().toArray.map {
      case k: Array[Byte] =>
        Base58.encode(k) -> blockDifficulties.get(k).toString
    }.toMap
    m.asJson
  }

  lazy val powDifficulty = getPoWDifficulty(None)
  lazy val posDifficulty = getPoSDifficulty(bestPosBlock.id)

  log.debug(s"Initialized block storage with version ${blocksStorage.lastVersionID}")

  def isGenesis(b: HybridPersistentNodeViewModifier): Boolean = b.parentId sameElements settings.GenesisParentId

  //chain without brothers
  override def toString: String = {
    chainBack(bestPosBlock, isGenesis, powOnly = false).get.map(_._2).map(Base58.encode).mkString(",")
  }

  /**
    * Go back though chain and get block ids until condition until
    */
  @tailrec
  private def chainBack(m: HybridPersistentNodeViewModifier,
                        until: HybridPersistentNodeViewModifier => Boolean,
                        limit: Int = Int.MaxValue,
                        powOnly: Boolean = false,
                        acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    val summ: Seq[(ModifierTypeId, ModifierId)] = if (m.isInstanceOf[PosBlock]) (PosBlock.ModifierTypeId -> m.id) +: acc
    else if (powOnly) acc
    else (PowBlock.ModifierTypeId -> m.id) +: acc

    if (limit <= 0 || until(m)) {
      Some(summ)
    } else {
      val parentId = m match {
        case b: PosBlock => b.parentId
        case b: PowBlock => b.prevPosId
      }
      modifierById(parentId) match {
        case Some(parent) => chainBack(parent, until, limit - 1, powOnly, summ)
        case _ =>
          log.warn(s"Parent block ${Base58.encode(parentId)} for ${Base58.encode(m.id)} not found ")
          None
      }
    }
  }

  //TODO limit???
  /**
    * find common suffixes for two chains - starting from forkBlock and from bestPowBlock
    * returns last common block and then variant blocks for two chains,
    */
  final def commonBlockThenSuffixes(forkBlock: HybridPersistentNodeViewModifier,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {
    val loserChain = chainBack(bestPowBlock, isGenesis, limit).get.map(_._2)
    def in(m: HybridPersistentNodeViewModifier): Boolean = loserChain.exists(s => s sameElements m.id)
    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)
    val i = loserChain.indexWhere(id => id sameElements winnerChain.head)
    (winnerChain, loserChain.takeRight(loserChain.length - i))
  }.ensuring(r => r._1.head sameElements r._2.head)

}


object HybridHistory extends ScorexLogging {
  val DifficultyRecalcPeriod = 20

  def readOrGenerate(settings: MiningSettings): HybridHistory = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(dataDir, logDirOpt, settings)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: MiningConstants): HybridHistory = {
    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        blockStorage.close()
      }
    })

    val metaDb =
      DBMaker.fileDB(s"$dataDir/hidx")
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    new HybridHistory(blockStorage, metaDb, logDirOpt, settings, new BlockValidator(settings, FastCryptographicHash))
  }
}