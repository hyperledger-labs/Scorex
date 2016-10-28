package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DBMaker, Serializer}
import org.mapdb.QueueLong.Node.SERIALIZER
import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, HistoryComparisonResult, RollbackTo}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  *
  * @param settings
  */
class HybridHistory(settings: Settings)
  extends History[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HybridSyncInfo, HybridHistory] {

  override type NVCT = HybridHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
  val dataDir = dataDirOpt.get

  val iFile = new File(s"$dataDir/blocks")
  val blockStorage = new LSMStore(iFile)

  //metadata database - contains blockId -> height index and also
  // current score, last pow and pos block ids
  val metaDb =
  DBMaker.fileDB(s"$dataDir/hidx")
    .fileMmapEnableIfSupported()
    .closeOnJvmShutdown()
    .make()

  //block -> score correspondence, for now score == height; that's not very secure,
  //see http://bitcoin.stackexchange.com/questions/29742/strongest-vs-longest-chain-and-orphaned-blocks
  lazy val blockScores = metaDb.hashMap("hidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  //for now score = chain length; that's not very secure, see link above
  lazy val currentScore = metaDb.atomicLong("score").createOrOpen().get()

  lazy val lastPowId = metaDb.atomicVar("lastPow", Serializer.BYTE_ARRAY).createOrOpen().get()

  lazy val lastPosId = metaDb.atomicVar("lastPos", Serializer.BYTE_ARRAY).createOrOpen().get()

  lazy val lastPowBlock = {
    require(currentScore > 0, "History is empty")
    blockById(lastPowId).get.asInstanceOf[PowBlock]
  }

  lazy val lastPosBlock = {
    require(currentScore > 0, "History is empty")
    blockById(lastPosId).get.asInstanceOf[PosBlock]
  }

  lazy val pairCompleted: Boolean = {
    lastPosBlock.parentId sameElements lastPowId
  }


  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = currentScore <= 0

  override def blockById(blockId: BlockId): Option[HybridPersistentNodeViewModifier] = Try {
    Option(blockStorage.get(ByteArrayWrapper(blockId))).flatMap { bw =>
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

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: HybridPersistentNodeViewModifier):
  Try[(HybridHistory, Option[RollbackTo[HybridPersistentNodeViewModifier]])] = {

    block match {
      case powBlock: PowBlock => ???

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


//object EmptyHybridHistory extends HybridHistory
