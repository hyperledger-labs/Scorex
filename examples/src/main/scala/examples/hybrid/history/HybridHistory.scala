package examples.hybrid.history


import java.io.File

import examples.commons.{FileLogger, SimpleBoxTransaction}
import examples.hybrid.blocks._
import examples.hybrid.mining.HybridMiningSettings
import examples.hybrid.validation.{DifficultyBlockValidator, ParentBlockValidator, SemanticBlockValidator}
import io.iohk.iodb.LSMStore
import scorex.core.block.{Block, BlockValidator}
import scorex.core.consensus.History._
import scorex.core.consensus._
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.annotation.tailrec
import scala.util.{Failure, Try}

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
class HybridHistory(val storage: HistoryStorage,
                    settings: HybridMiningSettings,
                    validators: Seq[BlockValidator[HybridBlock]],
                    statsLogger: Option[FileLogger],
                    timeProvider: NetworkTimeProvider)
  extends History[HybridBlock, HybridSyncInfo, HybridHistory] with ScorexLogging {

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

  val height: Long = storage.height
  val bestPosId = storage.bestPosId
  val bestPowId = storage.bestPowId
  lazy val bestPosBlock = storage.bestPosBlock
  lazy val bestPowBlock = storage.bestPowBlock
  lazy val bestBlock = if (pairCompleted) bestPosBlock else bestPowBlock

  /**
    * Return specified number of PoW blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return PoW blocks, in reverse order (starting from the most recent one)
    */
  def lastPowBlocks(count: Int, startBlock: PowBlock): Seq[PowBlock] = if (isEmpty) {
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

    loop(startBlock)
  }

  def lastBlockIds(startBlock: HybridBlock, count: Int): Seq[ModifierId] = {
    chainBack(startBlock, isGenesis, count - 1).get.map(_._2)
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

  private def powBlockAppend(powBlock: PowBlock): (HybridHistory, ProgressInfo[HybridBlock]) = {
    val progress: ProgressInfo[HybridBlock] = if (isGenesis(powBlock)) {
      storage.update(powBlock, None, isBest = true)
      ProgressInfo(None, Seq(), Some(powBlock), Seq())
    } else {
      storage.heightOf(powBlock.parentId) match {
        case Some(_) =>
          val isBestBrother = (bestPosId sameElements powBlock.prevPosId) &&
            (bestPowBlock.brothersCount < powBlock.brothersCount)

          //potentially the best block, if its not a block in a fork containing invalid block
          val isBest: Boolean = storage.height == storage.parentHeight(powBlock) || isBestBrother

          val mod: ProgressInfo[HybridBlock] = if (isBest) {
            if (isGenesis(powBlock) ||
              ((powBlock.parentId sameElements bestPowId) && (powBlock.prevPosId sameElements bestPosId))) {
              log.debug(s"New best PoW block ${Base58.encode(powBlock.id)}")
              //just apply one block to the end
              ProgressInfo(None, Seq(), Some(powBlock), Seq())
            } else if (isBestBrother) {
              log.debug(s"New best brother ${Base58.encode(powBlock.id)}")
              //new best brother
              ProgressInfo(Some(powBlock.prevPosId), Seq(bestPowBlock), Some(powBlock), Seq())
            } else {
              //we're switching to a better chain, if it does not contain an invalid block
              bestForkChanges(powBlock)
            }
          } else {
            log.debug(s"New orphaned PoW block ${Base58.encode(powBlock.id)}")
            ProgressInfo(None, Seq(), None, Seq())
          }
          storage.update(powBlock, None, isBest)
          mod

        case None =>
          log.warn(s"No parent block ${powBlock.parentId} in history")
          ProgressInfo[HybridBlock](None, Seq[HybridBlock](), None, Seq())
      }
    }
    // require(modifications.toApply.exists(_.id sameElements powBlock.id))
    (new HybridHistory(storage, settings, validators, statsLogger, timeProvider), progress)
  }

  private def posBlockAppend(posBlock: PosBlock): (HybridHistory, ProgressInfo[HybridBlock]) = {
    val difficulties = calcDifficultiesForNewBlock(posBlock)
    val parent = modifierById(posBlock.parentId).get.asInstanceOf[PowBlock]
    val isBest = storage.height == storage.parentHeight(posBlock)

    val mod: ProgressInfo[HybridBlock] = if (!isBest) {
      log.debug(s"New orphaned PoS block ${Base58.encode(posBlock.id)}")
      ProgressInfo(None, Seq(), None, Seq())
    } else if (posBlock.parentId sameElements bestPowId) {
      log.debug(s"New best PoS block ${Base58.encode(posBlock.id)}")
      ProgressInfo(None, Seq(), Some(posBlock), Seq())
    } else if (parent.prevPosId sameElements bestPowBlock.prevPosId) {
      log.debug(s"New best PoS block with link to non-best brother ${Base58.encode(posBlock.id)}")
      //rollback to prevoius PoS block and apply parent block one more time
      //TODO to Apply should be Seq(parent, posBlock)
      ProgressInfo(Some(parent.prevPosId), Seq(bestPowBlock), Some(parent), Seq())
    } else {
      bestForkChanges(posBlock)
    }
    storage.update(posBlock, Some(difficulties), isBest)

    (new HybridHistory(storage, settings, validators, statsLogger, timeProvider), mod)
  }

  /**
    * @param block - block to append
    * @return
    */
  override def append(block: HybridBlock): Try[(HybridHistory, ProgressInfo[HybridBlock])] = Try {
    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")

    validators.map(_.validate(block)).foreach {
      case Failure(e) =>
        log.warn(s"Block validation failed", e)
        throw e
      case _ =>
    }

    val res: (HybridHistory, ProgressInfo[HybridBlock]) = block match {
      case powBlock: PowBlock => powBlockAppend(powBlock)
      case posBlock: PosBlock => posBlockAppend(posBlock)
    }

    log.info(s"History: block ${Base58.encode(block.id)} appended to chain with score ${storage.heightOf(block.id)}. " +
      s"Best score is ${storage.bestChainScore}. " +
      s"Pair: ${Base58.encode(storage.bestPowId)}|${Base58.encode(storage.bestPosId)}")
    statsLogger.foreach(l => l.appendString(timeProvider.time() + ":" +
      lastBlockIds(bestBlock, 50).map(Base58.encode).mkString(",")))
    res
  }

  def bestForkChanges(block: HybridBlock): ProgressInfo[HybridBlock] = {
    val parentId = storage.parentId(block)
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(parentId).get)
    log.debug(s"Processing fork for block ${Base58.encode(block.id)}: \n" +
      s"old: ${oldSuffix.map(Base58.encode)}\n" +
      s"new: ${newSuffix.map(Base58.encode)}")

    val rollbackPoint = newSuffix.headOption

    val newSuffixValid = !newSuffix.drop(1).map(storage.semanticValidity).contains(Invalid)

    if(newSuffixValid) {

      //update best links
      newSuffix.sliding(2, 1).foreach{p =>
        storage.updateBestChild(p(0), p(1))
      }

      // TODO: fixme, What should we do if `oldSuffix` is empty?
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)
      // TODO: fixme, What should we do if `newSuffix` is empty?
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)
      require(applyBlocks.nonEmpty)
      require(throwBlocks.nonEmpty)
      require(storage.bestChildId(modifierById(rollbackPoint.get).get).get sameElements applyBlocks.headOption.get.id)

      ProgressInfo[HybridBlock](rollbackPoint, throwBlocks, applyBlocks.headOption, Seq())
    } else {
      log.info(s"Orphaned block $block from invalid suffix")
      ProgressInfo(None, Seq(), None, Seq())
    }
  }

  private def calcDifficultiesForNewBlock(posBlock: PosBlock): (BigInt, Long) = {
    def bounded(newVal: BigInt, oldVal: BigInt): BigInt = if (newVal > oldVal * 2) oldVal * 2 else newVal

    val powHeight = storage.parentHeight(posBlock) / 2 + 1
    if (powHeight > DifficultyRecalcPeriod && powHeight % DifficultyRecalcPeriod == 0) {

      //recalc difficulties

      val lastPow = modifierById(posBlock.parentId).get.asInstanceOf[PowBlock]
      val powBlocks = lastPowBlocks(DifficultyRecalcPeriod, lastPow) //.ensuring(_.length == DifficultyRecalcPeriod)

      // TODO: fixme, What should we do if `powBlocksHead` is empty?
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      val powBlocksHead = powBlocks.head
      val realTime = lastPow.timestamp - powBlocksHead.timestamp
      val brothersCount = powBlocks.map(_.brothersCount).sum
      val expectedTime = (DifficultyRecalcPeriod + brothersCount) * settings.targetBlockDelay.toMillis
      val oldPowDifficulty = storage.getPoWDifficulty(Some(lastPow.prevPosId))

      val newPowDiffUnlimited = (oldPowDifficulty * expectedTime / realTime).max(BigInt(1L))
      val newPowDiff = bounded(newPowDiffUnlimited, oldPowDifficulty)

      val oldPosDifficulty = storage.getPoSDifficulty(lastPow.prevPosId)
      val newPosDiff = oldPosDifficulty * DifficultyRecalcPeriod / ((DifficultyRecalcPeriod + brothersCount) * settings.rParamX10 / 10)
      log.info(s"PoW difficulty changed at ${Base58.encode(posBlock.id)}: old $oldPowDifficulty, new $newPowDiff. " +
        s" last: $lastPow, head: $powBlocksHead | $brothersCount")
      log.info(s"PoS difficulty changed: old $oldPosDifficulty, new $newPosDiff")
      (newPowDiff, newPosDiff)
    } else {
      //Same difficulty as in previous block
      assert(modifierById(posBlock.parentId).isDefined, "Parent should always be in history")
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

  def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[ModifierIds] = {
    def inList(m: HybridBlock): Boolean = idInList(m.id) || isGenesis(m)

    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 sameElements id)

    //Look without limit for case difference between nodes is bigger then size
    chainBack(bestBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(_) =>
        log.warn("Found chain without ids form remote")
        None
      case _ => None
    }
  }

  override def continuationIds(info: HybridSyncInfo,
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    continuationIds(info.startingPoints, size)
  }

  override def syncInfo: HybridSyncInfo =
    HybridSyncInfo(
      answer = false,
      lastPowBlocks(HybridSyncInfo.MaxLastPowBlocks, bestPowBlock).map(_.id),
      bestPosId)

  @tailrec
  private def divergentSuffix(otherLastPowBlocks: Seq[ModifierId],
                              suffixFound: Seq[ModifierId] = Seq()): Seq[ModifierId] = {
    // TODO: fixme, What should we do if `otherLastPowBlocks` is empty? Could we return Seq[ModifierId]() in that case?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val head = otherLastPowBlocks.head
    val newSuffix = suffixFound :+ head
    modifierById(head) match {
      case Some(_) =>
        newSuffix
      case None => if (otherLastPowBlocks.length <= 1) {
        Seq()
      } else {
        // `otherLastPowBlocks.tail` is safe as its length is greater than 1
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val otherLastPowBlocksTail = otherLastPowBlocks.tail
        divergentSuffix(otherLastPowBlocksTail, newSuffix)
      }
    }
  }

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult = {
    val dSuffix = divergentSuffix(other.lastPowBlockIds.reverse)

    dSuffix.length match {
      case 0 =>
        log.warn(s"CompareNonsense: ${other.lastPowBlockIds.toList.map(Base58.encode)} at height $height}")
        Nonsense
      case 1 =>
        // `dSuffix.head` is safe as `dSuffix.length` is 1
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val dSuffixHead = dSuffix.head
        if (dSuffixHead sameElements bestPowId) {
          if (other.lastPosBlockId sameElements bestPosId) Equal
          else if (pairCompleted) Older
          else Younger
        }
        else Younger
      case _ =>
        // +1 to include common block
        // TODO: What would be a default value for `localSuffixLength` in order to remove the unsafe calls to get and tail?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val localSuffixLength = storage.heightOf(bestPowId).get - storage.heightOf(dSuffix.last).get
        val otherSuffixLength = dSuffix.length

        if (localSuffixLength < otherSuffixLength) Older
        else if (localSuffixLength == otherSuffixLength) Equal
        else Younger
    }
  }

  lazy val powDifficulty = storage.getPoWDifficulty(None)
  lazy val posDifficulty = storage.getPoSDifficulty(storage.bestPosBlock.id)

  private def isGenesis(b: HybridBlock): Boolean = storage.isGenesis(b)

  def blockGenerator(m: HybridBlock): PublicKey25519Proposition = m match {
    case p: PosBlock => p.generatorBox.proposition
    case p: PowBlock => p.generatorProposition
  }

  def generatorDistribution(): Map[PublicKey25519Proposition, Int] = {
    val map = collection.mutable.Map[PublicKey25519Proposition, Int]().withDefaultValue(0)

    @tailrec
    def loop(m: HybridBlock): Unit = {
      val generator = blockGenerator(m)
      map.update(generator, map(generator) + 1)
      parentBlock(m) match {
        case Some(parent) => loop(parent)
        case None =>
      }
    }

    loop(bestBlock)
    map.toMap
  }

  def count(f: (HybridBlock => Boolean)): Int = filter(f).length

  def filter(f: (HybridBlock => Boolean)): Seq[ModifierId] = {
    @tailrec
    def loop(m: HybridBlock, acc: Seq[ModifierId]): Seq[ModifierId] = parentBlock(m) match {
      case Some(parent) => if (f(m)) loop(parent, m.id +: acc) else loop(parent, acc)
      case None => if (f(m)) m.id +: acc else acc
    }

    loop(bestBlock, Seq())
  }

  def parentBlock(m: HybridBlock): Option[HybridBlock] = m match {
    case b: PosBlock => modifierById(b.parentId)
    case b: PowBlock => modifierById(b.prevPosId)
  }

  /**
    * Go back though chain and get block ids until condition until
    * None if parent block is not in chain
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
      parentBlock(m) match {
        case Some(parent) => chainBack(parent, until, limit - 1, sum)
        case _ =>
          log.warn(s"Parent block for ${Base58.encode(m.id)} not found ")
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
    val i = loserChain.indexWhere { id =>
      winnerChain.headOption match {
        case None => false
        case Some(winnerChainHead) => id sameElements winnerChainHead
      }
    }
    (winnerChain, loserChain.takeRight(loserChain.length - i))
  } ensuring { r =>
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val r1 = r._1.head
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val r2 = r._2.head
    r1 sameElements r2
  }

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    * Debug only
    */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def averageDelay(id: ModifierId, blockNum: Int): Try[Long] = Try {
    val block = modifierById(id).get
    val c = chainBack(block, isGenesis, blockNum).get.map(_._2)
    (block.timestamp - modifierById(c.head).get.timestamp) / c.length
  }

  //chain without brothers
  override def toString: String = {
    chainBack(storage.bestPosBlock, isGenesis).get.map(_._2).map(Base58.encode).mkString(",")
  }

  override def reportSemanticValidity(modifier: HybridBlock,
                                      valid: Boolean,
                                      lastApplied: ModifierId): (HybridHistory, ProgressInfo[HybridBlock]) = {
    val v = if (valid) Valid else Invalid
    storage.updateValidity(modifier, v)

    val h = new HybridHistory(storage, settings, validators, statsLogger, timeProvider)

    if (valid) {
      val p = ProgressInfo(None, Seq(), storage.bestChildId(modifier).flatMap(storage.modifierById), Seq())
      h -> p
    } else {
      h -> ProgressInfo(None, Seq(), None, Seq())
    }
  }

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    storage.semanticValidity(modifierId)
}


object HybridHistory extends ScorexLogging {
  val DifficultyRecalcPeriod = 20

  def readOrGenerate(settings: ScorexSettings, minerSettings: HybridMiningSettings, timeProvider: NetworkTimeProvider): HybridHistory = {
    readOrGenerate(settings.dataDir, settings.logDir, minerSettings, timeProvider)
  }

  def readOrGenerate(dataDir: File, logDir: File, settings: HybridMiningSettings, timeProvider: NetworkTimeProvider): HybridHistory = {
    val iFile = new File(s"${dataDir.getAbsolutePath}/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

    val logger = new FileLogger(logDir.getAbsolutePath + "/tails.data")

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        blockStorage.close()
      }
    })

    val storage = new HistoryStorage(blockStorage, settings)
    val validators = Seq(new DifficultyBlockValidator(settings, storage),
      new ParentBlockValidator(storage),
      new SemanticBlockValidator(Blake2b256)
    )

    new HybridHistory(storage, settings, validators, Some(logger), timeProvider)
  }
}