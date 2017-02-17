package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks._
import examples.hybrid.mining.{MiningConstants, MiningSettings}
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.validation.{DifficultyBlockValidator, ParentBlockValidator, SemanticBlockValidator}
import io.iohk.iodb.LSMStore
import org.mapdb.DBMaker
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.{Block, BlockValidator}
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, RollbackInfo}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.{Failure, Try}

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
class HybridHistory(storage: HistoryStorage,
                    settings: MiningConstants,
                    validators: Seq[BlockValidator[HybridBlock]])
  extends History[PublicKey25519Proposition,
    SimpleBoxTransaction,
    HybridBlock,
    HybridSyncInfo,
    HybridHistory] with ScorexLogging {

  import HybridHistory._

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  lazy val pairCompleted: Boolean =
    (storage.bestPowId sameElements settings.GenesisParentId, bestPosId sameElements settings.GenesisParentId) match {
      case (true, true) => true
      case (false, true) => false
      case (false, false) => bestPosBlock.parentId sameElements bestPowId
      case (true, false) => ??? //shouldn't be
    }

  val height = storage.height
  val bestPosId = storage.bestPosId
  val bestPowId = storage.bestPowId
  lazy val bestPosBlock = storage.bestPosBlock
  lazy val bestPowBlock = storage.bestPowBlock
  lazy val bestBlock = if (bestPowBlock.prevPosId sameElements bestPosId) bestPowBlock else bestPosBlock

  /**
    * Return specified number of PoW blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return PoW blocks, in reverse order (starting from the most recent one)
    */
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

  override def modifierById(id: ModifierId): Option[HybridBlock with
    Block[PublicKey25519Proposition, SimpleBoxTransaction]] = storage.modifierById(id)

  override def contains(id: ModifierId): Boolean =
    if (id sameElements settings.GenesisParentId) true else modifierById(id).isDefined

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: HybridBlock):
  Try[(HybridHistory, RollbackInfo[HybridBlock])] = Try {
    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    val validationResuls = validators.map(_.validate(block))
    validationResuls.foreach {
      case Failure(e) => log.warn(s"Block validation failed", e)
      case _ =>
    }
    validationResuls.foreach(_.get)
    val res: (HybridHistory, RollbackInfo[HybridBlock]) = block match {
      case powBlock: PowBlock =>
        val modifications: RollbackInfo[HybridBlock] = if (isGenesis(powBlock)) {
          storage.update(powBlock, None, isBest = true)
          RollbackInfo(powBlock.parentId, Seq(), Seq(powBlock))
        } else {
          storage.heightOf(powBlock.parentId) match {
            case Some(parentHeight) =>
              val isBestBrother = (bestPosId sameElements powBlock.prevPosId) &&
                (bestPowBlock.brothersCount < powBlock.brothersCount)

              val isBest: Boolean = storage.height == storage.parentHeight(powBlock) || isBestBrother

              val mod: RollbackInfo[HybridBlock] = if (isBest) {
                if (isGenesis(powBlock) ||
                  ((powBlock.parentId sameElements bestPowId) && (powBlock.prevPosId sameElements bestPosId))) {
                  log.debug(s"New best PoW block ${Base58.encode(powBlock.id)}")
                  //just apply one block to the end
                  RollbackInfo(powBlock.prevPosId, Seq(), Seq(powBlock))
                } else if (isBestBrother) {
                  log.debug(s"New best brother ${Base58.encode(powBlock.id)}")
                  //new best brother
                  RollbackInfo(powBlock.prevPosId, Seq(bestPowBlock), Seq(powBlock))
                } else {
                  bestForkChanges(powBlock)
                }
              } else {
                log.debug(s"New orphaned PoW block ${Base58.encode(powBlock.id)}")
                RollbackInfo(powBlock.prevPosId, Seq(), Seq())
              }
              storage.update(powBlock, None, isBest)
              mod

            case None =>
              log.warn("No parent block in history")
              ???
          }
        }
        //        require(modifications.toApply.exists(_.id sameElements powBlock.id))
        (new HybridHistory(storage, settings, validators), modifications)


      case posBlock: PosBlock =>
        val difficulties = calcDifficultiesForNewBlock(posBlock)
        val parent = modifierById(posBlock.parentId).get.asInstanceOf[PowBlock]
        val isBest = storage.height == storage.parentHeight(posBlock)

        val mod: RollbackInfo[HybridBlock] = if (!isBest) {
          log.debug(s"New orphaned PoS block ${Base58.encode(posBlock.id)}")
          RollbackInfo(posBlock.parentId, Seq(), Seq())
        } else if (posBlock.parentId sameElements bestPowId) {
          log.debug(s"New best PoS block ${Base58.encode(posBlock.id)}")
          RollbackInfo(posBlock.parentId, Seq(), Seq(posBlock))
        } else if (parent.prevPosId sameElements bestPowBlock.prevPosId) {
          log.debug(s"New best PoS block with link to non-best brother ${Base58.encode(posBlock.id)}")
          //rollback to prevoius PoS block and apply parent block one more time
          RollbackInfo(parent.prevPosId, Seq(bestPowBlock), Seq(parent, posBlock))
        } else {
          bestForkChanges(posBlock)
        }
        storage.update(posBlock, Some(difficulties), isBest)

        (new HybridHistory(storage, settings, validators), mod)
    }
    log.info(s"History: block ${Base58.encode(block.id)} appended to chain with score ${storage.heightOf(block.id)}. " +
      s"Best score is ${storage.bestChainScore}. " +
      s"Pair: ${Base58.encode(storage.bestPowId)}|${Base58.encode(storage.bestPosId)}")
    res
  }

  def bestForkChanges(block: HybridBlock): RollbackInfo[HybridBlock] = {
    val parentId = storage.parentId(block)
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(parentId).get)
    log.debug(s"Processing fork for block ${Base58.encode(block.id)}: \n" +
      s"old: ${oldSuffix.map(Base58.encode)}\n" +
      s"new: ${newSuffix.map(Base58.encode)}")

    val rollbackPoint = newSuffix.head

    val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)
    val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)
    require(applyBlocks.nonEmpty)
    require(throwBlocks.nonEmpty)

    RollbackInfo[HybridBlock](rollbackPoint, throwBlocks, applyBlocks)
  }

  private def calcDifficultiesForNewBlock(posBlock: PosBlock): (BigInt, Long) = {
    def bounded(newVal: BigInt, oldVal: BigInt): BigInt = if (newVal > oldVal * 2) oldVal * 2 else newVal

    val powHeight = storage.parentHeight(posBlock) / 2 + 1
    if (powHeight > DifficultyRecalcPeriod && powHeight % DifficultyRecalcPeriod == 0) {

      //recalc difficulties
      val powBlocks = lastPowBlocks(DifficultyRecalcPeriod) //.ensuring(_.length == DifficultyRecalcPeriod)

      val realTime = powBlocks.last.timestamp - powBlocks.head.timestamp
      val brothersCount = powBlocks.map(_.brothersCount).sum
      val expectedTime = (DifficultyRecalcPeriod + brothersCount) * settings.targetBlockDelay
      val oldPowDifficulty = storage.getPoWDifficulty(Some(powBlocks.last.prevPosId))

      val newPowDiffUnlimited = (oldPowDifficulty * expectedTime / realTime).max(BigInt(1L))
      val newPowDiff = bounded(newPowDiffUnlimited, oldPowDifficulty)

      val oldPosDifficulty = storage.getPoSDifficulty(powBlocks.last.prevPosId)
      val newPosDiff = oldPosDifficulty * DifficultyRecalcPeriod / ((DifficultyRecalcPeriod + brothersCount) * 8 / 10)
      log.info(s"PoW difficulty changed at ${Base58.encode(posBlock.id)}: old $oldPowDifficulty, new $newPowDiff. " +
        s" ${powBlocks.last.timestamp} - ${powBlocks.head.timestamp} | $brothersCount")
      log.info(s"PoS difficulty changed: old $oldPosDifficulty, new $newPosDiff")
      (newPowDiff, newPosDiff)
    } else {
      //Same difficulty as in previous block
      val parentPoSId: ModifierId = modifierById(posBlock.parentId).get.asInstanceOf[PowBlock].prevPosId
      (storage.getPoWDifficulty(Some(parentPoSId)), storage.getPoSDifficulty(parentPoSId))
    }
  }

  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) Seq(settings.GenesisParentId)
    else if (pairCompleted) Seq(bestPowId, bestPosId)
    else Seq(bestPowId)

  override def applicable(block: HybridBlock): Boolean = {
    block match {
      case pwb: PowBlock =>
        contains(pwb.parentId) && contains(pwb.prevPosId)
      case psb: PosBlock =>
        contains(psb.parentId)
    }
  }

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)],
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    def inList(m: HybridBlock): Boolean = idInList(m.id) || isGenesis(m)
    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 sameElements id)

    //Look without limit for case difference between nodes is bigger then size
    chainBack(bestBlock, inList) match {
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

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult.Value = {
    val dSuffix = divergentSuffix(other.lastPowBlockIds.reverse)

    dSuffix.length match {
      case 0 =>
        log.warn(s"CompareNonsense: ${other.lastPowBlockIds.toList.map(Base58.encode)} at height $height}")
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
        val localSuffixLength = height - storage.heightOf(dSuffix.last).get + 1
        val otherSuffixLength = dSuffix.length

        if (localSuffixLength < otherSuffixLength)
          HistoryComparisonResult.Older
        else if (localSuffixLength == otherSuffixLength)
          HistoryComparisonResult.Equal
        else HistoryComparisonResult.Younger
    }
  }

  lazy val powDifficulty = storage.getPoWDifficulty(None)
  lazy val posDifficulty = storage.getPoSDifficulty(storage.bestPosBlock.id)

  private def isGenesis(b: HybridBlock): Boolean = storage.isGenesis(b)

  /**
    * Go back though chain and get block ids until condition until
    */
  @tailrec
  private def chainBack(m: HybridBlock,
                        until: HybridBlock => Boolean,
                        limit: Int = Int.MaxValue,
                        acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    val sum: Seq[(ModifierTypeId, ModifierId)] = if (m.isInstanceOf[PosBlock]) (PosBlock.ModifierTypeId -> m.id) +: acc
    else (PowBlock.ModifierTypeId -> m.id) +: acc

    if (limit <= 0 || until(m)) {
      Some(sum)
    } else {
      val parentId = m match {
        case b: PosBlock => b.parentId
        case b: PowBlock => b.prevPosId
      }
      modifierById(parentId) match {
        case Some(parent) => chainBack(parent, until, limit - 1, sum)
        case _ =>
          log.warn(s"Parent block ${Base58.encode(parentId)} for ${Base58.encode(m.id)} not found ")
          None
      }
    }
  }

  /**
    * find common suffixes for two chains - starting from forkBlock and from bestPowBlock
    * returns last common block and then variant blocks for two chains,
    */
  final def commonBlockThenSuffixes(forkBlock: HybridBlock,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {
    val loserChain = chainBack(bestBlock, isGenesis, limit).get.map(_._2)

    def in(m: HybridBlock): Boolean = loserChain.exists(s => s sameElements m.id)
    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)
    val i = loserChain.indexWhere(id => id sameElements winnerChain.head)
    (winnerChain, loserChain.takeRight(loserChain.length - i))
  }.ensuring(r => r._1.head sameElements r._2.head)

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    * Debug only
    */
  def averageDelay(id: ModifierId, blockNum: Int): Try[Long] = Try {
    val block = modifierById(id).get
    val c = chainBack(block, isGenesis, blockNum).get.map(_._2)
    (block.timestamp - modifierById(c.head).get.timestamp) / c.length
  }

  //chain without brothers
  override def toString: String = {
    chainBack(storage.bestPosBlock, isGenesis).get.map(_._2).map(Base58.encode).mkString(",")
  }

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

    val storage = new HistoryStorage(blockStorage, metaDb, settings)
    val validators = Seq(new DifficultyBlockValidator(settings, storage),
      new ParentBlockValidator(storage),
      new SemanticBlockValidator(FastCryptographicHash)
    )

    new HybridHistory(storage, settings, validators)
  }
}