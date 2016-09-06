package scorex.core.consensus

import scorex.core.NodeViewComponent
import scorex.core.transaction.{NodeStateModifier, PersistentNodeStateModifier, Transaction}
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

trait History[P <: Proposition, TX <: Transaction[P, TX], M <: PersistentNodeStateModifier] extends NodeViewComponent {
  self =>

  type H >: self.type <: History[P, TX, M]

  type BlockId = NodeStateModifier.ModifierId

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  def isEmpty: Boolean

  def contains(block: M): Boolean = contains(block.id())

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: BlockId): Option[M]

  def blockById(blockId: String): Option[M] = Base58.decode(blockId).toOption.flatMap(blockById)

  def append(block: M): Try[History[P, TX, M]]

  def append(blocks: Seq[M]): Try[History[P, TX, M]]

  //todo: should be ID | Seq[ID]
  def openSurface(): Seq[BlockId]

  //todo: arg should be ID | Seq[ID]
  def continuation(openSurface: Seq[BlockId], size: Int): Seq[M]

  //todo: arg should be ID | Seq[ID]
  def continuationIds(openSurface: Seq[BlockId], size: Int): Seq[BlockId]

  def maxSize(): Int
}