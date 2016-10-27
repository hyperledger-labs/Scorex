package examples.hybrid.history


import java.io.File

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock, PowBlockCompanion}
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.{NodeViewComponentCompanion, NodeViewModifier}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.History
import scorex.core.consensus.History.{BlockId, RollbackTo}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

class HybridHistory(settings: Settings)
  extends History[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HybridSyncInfo, HybridHistory] {

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
  val dataDir = dataDirOpt.get

  /*
  val powDir = new File(dataDir + "/pow")
  powDir.mkdirs()

  val posDir = new File(dataDir + "/pos")
  posDir.mkdirs()*/

  val iFile = new File(s"$dataDir/blocksindexes")
  val blockStorage = new LSMStore(iFile)

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = blockStorage.lastVersion <= 0

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

  override def append(block: HybridPersistentNodeViewModifier): Try[(HybridHistory, Option[RollbackTo[HybridPersistentNodeViewModifier]])] = ???

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
  override def compare(other: HybridSyncInfo): _root_.scorex.core.consensus.History.HistoryComparisonResult.Value = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
