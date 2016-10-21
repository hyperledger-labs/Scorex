package scorex.core.consensus

import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.{NodeViewComponent, NodeViewModifier}
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * History of a blockchain system is some blocktree in fact
  * (like this: http://image.slidesharecdn.com/sfbitcoindev-chepurnoy-2015-150322043044-conversion-gate01/95/proofofstake-its-improvements-san-francisco-bitcoin-devs-hackathon-12-638.jpg),
  * where longest chain is being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead, even in PoW systems.
  */

trait History[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends NodeViewComponent {

  import History._

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  def isEmpty: Boolean

  def contains(block: PM): Boolean = contains(block.id)

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: BlockId): Option[PM]

  def blockById(blockId: String): Option[PM] = Base58.decode(blockId).toOption.flatMap(blockById)

  def append(block: PM): Try[(HT, Option[RollbackTo[PM]])]

  //todo: is it needed?
  //todo: output should be ID | Seq[ID]
  def openSurfaceIds(): Seq[BlockId]

  //todo: argument should be ID | Seq[ID]
  def continuation(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[PM]]

  //todo: argument should be ID | Seq[ID]
  def continuationIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[Seq[(ModifierTypeId, ModifierId)]]

  def syncInfo(answer: Boolean): SI

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult.Value
}

object History {
  type BlockId = NodeViewModifier.ModifierId

  object HistoryComparisonResult extends Enumeration {
    val Equal = Value(1)
    val Younger = Value(2)
    val Older = Value(3)
  }

  case class RollbackTo[PM <: PersistentNodeViewModifier[_, _]](to: BlockId, thrown: Seq[PM], applied: Seq[PM])

}