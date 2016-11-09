package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks._
import examples.hybrid.mining.{MiningSettings, PosForger, PowMiner}
import examples.hybrid.state.SimpleBoxTransaction
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, HistoryComparisonResult, RollbackTo}
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
class HybridHistory(blocksStorage: LSMStore, metaDb: DB)
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

  override def blockById(blockId: BlockId): Option[HybridPersistentNodeViewModifier] = Try {
    Option(blocksStorage.get(ByteArrayWrapper(blockId))).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      (mtypeId match {
        case t: Byte if t == PowBlock.ModifierTypeId =>
          PowBlockCompanion.parse(bytes.tail)
        case t: Byte if t == PosBlock.ModifierTypeId =>
          PosBlockCompanion.parse(bytes.tail)
      }).toOption
    }
  }.getOrElse(None)

  override def contains(id: BlockId): Boolean =
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


  //find an id for common parent of two pow blocks with given ids
  //for parameters, ids are going from genesis to current block

  @tailrec
  private def suffixesAfterCommonBlock(winnerChain: Seq[BlockId], loserChain: Seq[BlockId]): (Seq[BlockId], Seq[BlockId]) = {
    val c1h = winnerChain.head
    val c2h = loserChain.head

    if (loserChain.contains(c1h)) {
      (winnerChain, loserChain.dropWhile(id => !(id sameElements c2h)))
    } else if (winnerChain.contains(c2h)) {
      (winnerChain.dropWhile(id => !(id sameElements c2h)), loserChain)
    } else suffixesAfterCommonBlock(blockById(c1h).get.parentId +: winnerChain, blockById(c2h).get.parentId +: loserChain)
  }

  private def writeBlock(b: HybridPersistentNodeViewModifier) = {
    val typeByte = b match {
      case _: PowBlock =>
        PowBlock.ModifierTypeId
      case _: PosBlock =>
        PosBlock.ModifierTypeId
    }

    /*
        blocksStorage.update(
          blocksStorage.lastVersion + 1,
          Seq(),
          Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
    */
    ???
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
              val (newSuffix, oldSuffix) = suffixesAfterCommonBlock(Seq(powBlock.parentId), Seq(bestPowBlock.parentId, bestPowId))
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
            forwardPowLinks.put(powBlock.parentId, blockId)
            None
          }
        }
        (new HybridHistory(blocksStorage, metaDb), rollbackOpt)


      case posBlock: PosBlock =>
        checkPoSConsensusRules(posBlock)

        val powParent = posBlock.parentId

        val blockId = posBlock.id

        forwardPosLinks.put(powParent, blockId)

        writeBlock(posBlock)

        if (powParent sameElements bestPowId) bestPosIdVar.set(blockId)

        //recalc difficulties
        if (currentScoreVar.get() > 0 && currentScoreVar.get() % DifficultyRecalcPeriod == 0) recalcDifficulties()
        (new HybridHistory(blocksStorage, metaDb), None) //no rollback ever
    }
    metaDb.commit()
    log.info(s"History: block appended, new score is ${currentScoreVar.get()}")
    res
  }

  override def openSurfaceIds(): Seq[BlockId] =
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
}


object HybridHistory {
  val DifficultyRecalcPeriod = 20

  def emptyHistory(settings: Settings): HybridHistory = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    val metaDb =
      DBMaker.fileDB(s"$dataDir/hidx")
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    new HybridHistory(blockStorage, metaDb)
  }
}


//todo: convert to a test
object HistoryPlayground extends App {
  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

  val b = PowBlock(PowMiner.GenesisParentId, PowMiner.GenesisParentId, 1478164225796L, -308545845552064644L, 0, Array.fill(32)(0: Byte), Seq())

  val h = HybridHistory.emptyHistory(settings)

  val h2 = h.append(b).get._1

  assert(h2.blockById(b.id).isDefined)

  val priv1 = PrivateKey25519Companion.generateKeys(Array.fill(32)(0: Byte))
  val priv2 = PrivateKey25519Companion.generateKeys(Array.fill(32)(1: Byte))

  val from = IndexedSeq(priv1._1 -> 500L, priv2._1 -> 1000L)
  val to = IndexedSeq(priv2._2 -> 1500L)
  val fee = 500L
  val timestamp = System.currentTimeMillis()

  val tx = SimpleBoxTransaction(from, to, fee, timestamp)

  val posBlock = PosBlock(b.id, 0L, Seq(tx), priv1._2, Signature25519(Array.fill(Signature25519.SignatureSize)(0: Byte)))

  val h3 = h2.append(posBlock).get._1

  assert(h3.blockById(posBlock.id).isDefined)
}