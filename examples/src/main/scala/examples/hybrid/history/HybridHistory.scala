package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.mining.PowMiner
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, HistoryComparisonResult, RollbackTo}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.annotation.tailrec
import scala.util.Try

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
//todo: add some versioned field to the class
class HybridHistory(blocksStorage: LSMStore, metaDb: DB)
  extends History[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HybridSyncInfo, HybridHistory] {

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")


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
  lazy val currentScore = currentScoreVar.get()

  private lazy val bestPowIdVar = metaDb.atomicVar("lastPow", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPowId = bestPowIdVar.get()

  private lazy val bestPosIdVar = metaDb.atomicVar("lastPos", Serializer.BYTE_ARRAY).createOrOpen()
  lazy val bestPosId = bestPosIdVar.get()

  lazy val lastPowBlock = {
    require(currentScore > 0, "History is empty")
    blockById(bestPowId).get.asInstanceOf[PowBlock]
  }

  lazy val lastPosBlock = {
    require(currentScore > 0, "History is empty")
    blockById(bestPosId).get.asInstanceOf[PosBlock]
  }

  lazy val pairCompleted: Boolean = {
    lastPosBlock.parentId sameElements bestPowId
  }


  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = currentScore <= 0

  override def blockById(blockId: BlockId): Option[HybridPersistentNodeViewModifier] = Try {
    Option(blocksStorage.get(ByteArrayWrapper(blockId))).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head
      (mtypeId match {
        case t: Byte if t == PowBlock.ModifierTypeId =>
          PowBlockCompanion.parse(bytes.tail)
        case t: Byte if t == PosBlock.ModifierTypeId =>
          PowBlockCompanion.parse(bytes.tail)
      }).toOption
    }
  }.getOrElse(None)

  //PoW consensus rules checks, work/references
  //throws exception if anything wrong
  def checkPowConsensusRules(powBlock: PowBlock): Unit = {
    //check work
    assert(powBlock.correctWork, "work done is incorrent")

    //check PoW parent id
    blockById(powBlock.parentId).get

    //check referenced PoS block exists as well
    val posBlock = blockById(powBlock.prevPosId).get

    //check referenced PoS block points to parent PoW block
    assert(posBlock.parentId sameElements posBlock.parentId, "ref rule broken")
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

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: HybridPersistentNodeViewModifier):
  Try[(HybridHistory, Option[RollbackTo[HybridPersistentNodeViewModifier]])] = Try {
    block match {
      case powBlock: PowBlock =>

        val score = if (!(powBlock.parentId sameElements PowMiner.GenesisParentId)) {
          checkPowConsensusRules(powBlock)
          blockScores.get(powBlock.parentId): Long
        } else 0L

        //update block storage and the scoring index
        val blockId = powBlock.id

        blocksStorage.update(
          blocksStorage.lastVersion + 1,
          Seq(),
          Seq(ByteArrayWrapper(blockId) -> ByteArrayWrapper(powBlock.bytes)))

        val blockScore = score + 1
        blockScores.put(blockId, blockScore)

        val rollbackOpt: Option[RollbackTo[HybridPersistentNodeViewModifier]] = if (powBlock.parentId sameElements PowMiner.GenesisParentId) {
          //genesis block
          currentScoreVar.set(blockScore)
          bestPowIdVar.set(blockId)
          None
        } else {
          if (blockScore > currentScore) {
            //check for chain switching
            if (!(powBlock.parentId sameElements bestPowId)) {
              val (newSuffix, oldSuffix) = suffixesAfterCommonBlock(Seq(powBlock.parentId), Seq(lastPowBlock.parentId, bestPowId))
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
          } else {
            forwardPowLinks.put(powBlock.parentId, blockId)
            None
          }
        }
        (new HybridHistory(blocksStorage, metaDb), rollbackOpt)
      case posBlock: PosBlock => ???
    }
  }

  //todo: is it needed?
  override def openSurfaceIds(): Seq[BlockId] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[HybridPersistentNodeViewModifier]] = ???

  //todo: argument should be ID | Seq[ID]
  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = ???

  override def syncInfo(answer: Boolean): HybridSyncInfo = ???

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: HybridSyncInfo): HistoryComparisonResult.Value = ???

  override def companion: NodeViewComponentCompanion = ???
}


object HybridHistory {
  def emptyHistory(settings: Settings): HybridHistory = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    val iFile = new File(s"$dataDir/blocks")
    val blockStorage = new LSMStore(iFile)

    val metaDb =
      DBMaker.fileDB(s"$dataDir/hidx")
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    new HybridHistory(blockStorage, metaDb)
  }
}