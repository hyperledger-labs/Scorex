package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks._
import examples.hybrid.mining.{MiningSettings, PosForger, PowMiner}
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.util.FileFunctions
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, RollbackTo}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
//todo: add some versioned field to the class
class HybridHistory(blocksStorage: LSMStore, metaDb: DB, logDirOpt: Option[String])
  extends History[PublicKey25519Proposition,
    SimpleBoxTransaction,
    HybridPersistentNodeViewModifier,
    HybridSyncInfo,
    HybridHistory] with ScorexLogging {

  import HybridHistory._

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  private lazy val powDifficultyVar = metaDb.atomicString("powdiff", PowMiner.Difficulty.toString()).createOrOpen()

  private lazy val posDifficultyVar = metaDb.atomicLong("posdiff", PosForger.InitialDifficuly).createOrOpen()

  lazy val powDifficulty = BigInt(powDifficultyVar.get())

  lazy val posDifficulty = posDifficultyVar.get()

  //block -> score correspondence, for now score == height; that's not very secure,
  //see http://bitcoin.stackexchange.com/questions/29742/strongest-vs-longest-chain-and-orphaned-blocks
  lazy val blockScores = metaDb.hashMap("hidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  //reverse index: only link to parent is stored in a block
  //so to go forward along the chains we need for additional indexes

  //links from a pow block to next pow block(in a best chain)
  lazy val forwardPowLinks = metaDb.hashMap("fpow", Serializer.BYTE_ARRAY, Serializer.BYTE_ARRAY).createOrOpen()

  //links from a pow block to a corresponding pos block(for all the chains)
  lazy val forwardPosLinks = metaDb.hashMap("fpos", Serializer.BYTE_ARRAY, Serializer.BYTE_ARRAY).createOrOpen()

  //for now score = chain length; that's not very secure, see link above
  private lazy val currentScoreVar = metaDb.atomicLong("score").createOrOpen()

  lazy val powHeight = currentScoreVar.get()

  lazy val orphanCountVar = metaDb.atomicLong("orphans", 0L).createOrOpen()

  private lazy val bestPowIdVar = metaDb.atomicVar("lastPow", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPowId = Option(bestPowIdVar.get()).getOrElse(PowMiner.GenesisParentId)

  private lazy val bestPosIdVar = metaDb.atomicVar("lastPos", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPosId = Option(bestPosIdVar.get()).getOrElse(PowMiner.GenesisParentId)

  lazy val bestPowBlock = {
    require(currentScoreVar.get() > 0, "History is empty")
    blockById(bestPowId).get.asInstanceOf[PowBlock]
  }

  lazy val bestPosBlock = {
    require(currentScoreVar.get() > 0, "History is empty")
    blockById(bestPosId).get.asInstanceOf[PosBlock]
  }

  lazy val pairCompleted: Boolean =
    (bestPowId sameElements PowMiner.GenesisParentId, bestPosId sameElements PowMiner.GenesisParentId) match {
      case (true, true) => true
      case (false, true) => false
      case (false, false) => bestPosBlock.parentId sameElements bestPowId
      case (true, false) => ??? //shouldn't be
    }


  //a lot of crimes committed here: .get, .asInstanceOf
  def lastPowBlocks(count: Int): Seq[PowBlock] =
  (1 until count).foldLeft(Seq(bestPowBlock)) { case (blocks, _) =>
    blockById(blocks.head.parentId).get.asInstanceOf[PowBlock] +: blocks
  }

  //a lot of crimes committed here: .get, .asInstanceOf
  def lastPosBlocks(count: Int): Seq[PosBlock] =
  (1 until count).foldLeft(Seq(bestPosBlock)) { case (blocks, _) =>
    blockById(forwardPosLinks.get(blocks.head.parentId)).get.asInstanceOf[PosBlock] +: blocks
  }

  def recalcDifficulties(): Unit = {
    val powBlocks = lastPowBlocks(DifficultyRecalcPeriod)
    val realTime = powBlocks.last.timestamp - powBlocks.head.timestamp

    val brothersCount = powBlocks.map(_.brothersCount).sum

    val expectedTime = (DifficultyRecalcPeriod + brothersCount) * PowMiner.BlockDelay

    val newPowDiff = (powDifficulty * expectedTime / realTime).max(BigInt(1L))

    val newPosDiff = posDifficulty * DifficultyRecalcPeriod / ((DifficultyRecalcPeriod + brothersCount) * 8 / 10)

    log.info(s"PoW difficulty changed: old $powDifficulty, new $newPowDiff")
    log.info(s"PoS difficulty changed: old $posDifficulty, new $newPosDiff")

    powDifficultyVar.set(newPowDiff.toString())
    posDifficultyVar.set(newPosDiff)
  }

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = currentScoreVar.get() <= 0

  override def blockById(blockId: ModifierId): Option[HybridPersistentNodeViewModifier] = Try {
    Option(blocksStorage.get(ByteArrayWrapper(blockId))).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      (mtypeId match {
        case t: Byte if t == PowBlock.ModifierTypeId =>
          PowBlockCompanion.parseBytes(bytes.tail)
        case t: Byte if t == PosBlock.ModifierTypeId =>
          PosBlockCompanion.parseBytes(bytes.tail)
      }).toOption
    }
  }.getOrElse(None)

  override def contains(id: ModifierId): Boolean =
    if (id sameElements PowMiner.GenesisParentId) true else blockById(id).isDefined

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  def checkPowConsensusRules(powBlock: PowBlock): Unit = {
    //check work
    assert(powBlock.correctWork(powDifficulty), "work done is incorrent")

    //check PoW parent id
    blockById(powBlock.parentId).get

    //some check for header fields
    assert(powBlock.headerValid)

    //check brothers data
    assert(powBlock.brothers.size == powBlock.brothersCount)
    assert(powBlock.brothers.forall(_.correctWork(powDifficulty)))
    if (powBlock.brothersCount > 0) {
      assert(FastCryptographicHash(powBlock.brotherBytes) sameElements powBlock.brothersHash)
    }

    if (!(powBlock.parentId sameElements PowMiner.GenesisParentId)) {
      //check referenced PoS block exists as well
      val posBlock = blockById(powBlock.prevPosId).get

      //check referenced PoS block points to parent PoW block
      assert(posBlock.parentId sameElements posBlock.parentId, "ref rule broken")
    }
  }

  //PoS consensus rules checks, throws exception if anything wrong
  def checkPoSConsensusRules(posBlock: PosBlock): Unit = {
    //check PoW block exists
    blockById(posBlock.parentId).get

    //todo: check difficulty

    //todo: check signature

    //todo: check transactions

    //todo: check PoS rules
  }

  /**
    * find common suffixes for two chains
    * returns last common block and then variant blocks for two chains,
    * longer one and a loser
    */
  final def commonBlockThenSuffixes(winnerChain: Seq[ModifierId], loserChain: Seq[ModifierId]): (Seq[ModifierId], Seq[ModifierId]) = {

    val idx = loserChain.indexWhere(blockId => !winnerChain.exists(_.sameElements(blockId)))
    assert(idx != 0)

    val lc = if (idx == -1) loserChain.slice(loserChain.length - 1, loserChain.length)
    else loserChain.slice(idx - 1, loserChain.length)

    val wc = winnerChain.dropWhile(blockId => !blockId.sameElements(lc.head))

    (wc, lc)
  }

  private def writeBlock(b: HybridPersistentNodeViewModifier) = {
    val typeByte = b match {
      case _: PowBlock =>
        PowBlock.ModifierTypeId
      case _: PosBlock =>
        PosBlock.ModifierTypeId
    }

    blocksStorage.update(
      blocksStorage.lastVersion + 1,
      Seq(),
      Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
  }

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: HybridPersistentNodeViewModifier):
  Try[(HybridHistory, Option[RollbackTo[HybridPersistentNodeViewModifier]])] = Try {
    val res = block match {
      case powBlock: PowBlock =>

        val currentScore = if (!(powBlock.parentId sameElements PowMiner.GenesisParentId)) {
          checkPowConsensusRules(powBlock)
          blockScores.get(powBlock.parentId): Long
        } else {
          log.info("Genesis block: " + Base58.encode(powBlock.id))
          0L
        }

        //update block storage and the scores index
        val blockId = powBlock.id

        writeBlock(powBlock)

        val blockScore = currentScore + 1
        blockScores.put(blockId, blockScore)

        val rollbackOpt: Option[RollbackTo[HybridPersistentNodeViewModifier]] = if (powBlock.parentId sameElements PowMiner.GenesisParentId) {
          //genesis block
          currentScoreVar.set(blockScore)
          bestPowIdVar.set(blockId)
          forwardPowLinks.put(powBlock.parentId, blockId)
          None
        } else {
          if (blockScore > currentScoreVar.get()) {
            //check for chain switching
            if (!(powBlock.parentId sameElements bestPowId)) {
              val (newSuffix, oldSuffix) = commonBlockThenSuffixes(Seq(powBlock.parentId), Seq(bestPowBlock.parentId, bestPowId))

              //decrement
              orphanCountVar.addAndGet(oldSuffix.size - newSuffix.size)
              logDirOpt.foreach { logDir =>
                val record = s"${oldSuffix.size}, ${currentScoreVar.get}"
                FileFunctions.append(logDir + "/forkdepth.csv", record)
              }

              val rollbackPoint = newSuffix.head

              val throwBlocks = oldSuffix.tail.map(id => blockById(id).get)
              val applyBlocks = newSuffix.tail.map(id => blockById(id).get)

              oldSuffix.foreach(forwardPowLinks.remove)
              (0 until newSuffix.size - 1).foreach { idx =>
                forwardPowLinks.put(newSuffix(idx), blockId)
              }
              forwardPowLinks.put(powBlock.parentId, blockId)

              currentScoreVar.set(blockScore)
              bestPowIdVar.set(blockId)
              Some(RollbackTo(rollbackPoint, throwBlocks, applyBlocks))
            } else {
              currentScoreVar.set(blockScore)
              bestPowIdVar.set(blockId)
              forwardPowLinks.put(powBlock.parentId, blockId)
              None
            }
          } else if (blockScore == currentScoreVar.get() &&
            (bestPowBlock.parentId sameElements powBlock.parentId) &&
            (bestPowBlock.parentId sameElements powBlock.parentId) &&
            (bestPowBlock.brothersCount < powBlock.brothersCount)
          ) {
            //handle younger brother - replace current best PoW block with a brother
            val replacedBlock = bestPowBlock
            bestPowIdVar.set(blockId)
            forwardPowLinks.put(powBlock.parentId, blockId)
            Some(RollbackTo(powBlock.prevPosId, Seq(replacedBlock), Seq(powBlock)))
          } else {
            orphanCountVar.incrementAndGet()
            forwardPowLinks.put(powBlock.parentId, blockId)
            None
          }
        }
        logDirOpt.foreach { logDir =>
          val record = s"${orphanCountVar.get()}, ${currentScoreVar.get}"
          FileFunctions.append(logDir + "/orphans.csv", record)
        }
        (new HybridHistory(blocksStorage, metaDb, logDirOpt), rollbackOpt)


      case posBlock: PosBlock =>
        checkPoSConsensusRules(posBlock)

        val powParent = posBlock.parentId

        val blockId = posBlock.id

        forwardPosLinks.put(powParent, blockId)

        writeBlock(posBlock)

        if (powParent sameElements bestPowId) bestPosIdVar.set(blockId)

        //recalc difficulties
        if (currentScoreVar.get() > 0 && currentScoreVar.get() % DifficultyRecalcPeriod == 0) recalcDifficulties()
        (new HybridHistory(blocksStorage, metaDb, logDirOpt), None) //no rollback ever
    }
    metaDb.commit()
    log.info(s"History: block appended, new score is ${currentScoreVar.get()}")
    res
  }

  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) Seq(PowMiner.GenesisParentId)
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

  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[HybridPersistentNodeViewModifier]] = {
    continuationIds(from, size).map(_.map { case (_, mId) =>
      blockById(mId).get
    })
  }

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {
    val (modTypeId, modId) = from.head

    val startingBlock = if (modId sameElements PowMiner.GenesisParentId) {
      Option(forwardPowLinks.get(PowMiner.GenesisParentId))
        .flatMap(blockById)
    } else blockById(modId)

    startingBlock.map { b =>
      (1 to size).foldLeft(Seq(modTypeId -> b.id) -> true) { case ((collected, go), _) =>
        if (go) {
          val toAdd = blockById(collected.last._2).get match {
            case pb: PowBlock =>
              Option(forwardPosLinks.get(pb.id)).map(id => PosBlock.ModifierTypeId -> id)
            case ps: PosBlock =>
              Option(forwardPowLinks.get(ps.parentId)).map(id => PowBlock.ModifierTypeId -> id)
          }

          toAdd.map(t => collected :+ t).getOrElse(collected) -> toAdd.isDefined
        } else collected -> false
      }._1
    }
  }

  override def syncInfo(answer: Boolean): HybridSyncInfo =
    HybridSyncInfo(answer, bestPowId, bestPosId)

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult.Value = {
    //todo: check PoW header correctness, return cheater status for that
    //todo: return cheater status in other cases, e.g. PoW id is a correct PoS id

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
    }
  }

  override def companion: NodeViewComponentCompanion = ???
}


object HybridHistory extends ScorexLogging {
  val DifficultyRecalcPeriod = 20

  def readOrGenerate(settings: Settings): HybridHistory = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

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

    val logDirOpt = settings.logDirOpt
    new HybridHistory(blockStorage, metaDb, logDirOpt)
  }
}