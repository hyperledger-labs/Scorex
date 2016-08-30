package scorex.core.consensus

import scorex.core.NodeViewComponent
import scorex.core.block.{Block, ConsensusData}
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

trait History[P <: Proposition, TX <: Transaction[P, TX]] extends NodeViewComponent {
  type BlockId = ConsensusData.BlockId

  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  def isEmpty: Boolean

  def contains(block: Block[P, TX]): Boolean = contains(block.id)

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: BlockId): Option[Block[P, TX]]

  def blockById(blockId: String): Option[Block[P, TX]] = Base58.decode(blockId).toOption.flatMap(blockById)

  def appendBlock(block: Block[P, TX]): Try[History[P, TX]]
}