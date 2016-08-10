package scorex.consensus

import scorex.block.ConsensusData.BlockId
import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.crypto.encode.Base58
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition

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

trait History[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData] {

  type BlockId = ConsensusData.BlockId

  val consensusModule: ConsensusModule[P, CData]


  /**
    * Height of the a chain, or a longest chain in an explicit block-tree
    */
  def height(): Int

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    * @return
    */
  def score(): BigInt

  /**
    * Is there's no history, even genesis block
    * @return
    */
  def isEmpty: Boolean = height() == 0

  def contains(block: Block[P, TData, CData]): Boolean = contains(block.id)

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: BlockId): Option[Block[P, TData, CData]]

  def blockById(blockId: String): Option[Block[P, TData, CData]] = Base58.decode(blockId).toOption.flatMap(blockById)

  /**
    * Height of a block if it's in the blocktree
    */
  def heightOf(block: Block[P, TData, CData]): Option[Int] = heightOf(block.id)

  def heightOf(blockId: BlockId): Option[Int]

  def parent(block: Block[P, TData, CData], back: Int = 1): Option[Block[P, TData, CData]]

  def confirmations(block: Block[P, TData, CData]): Option[Int] = heightOf(block).map(height() - _)

  def generatedBy(id: P): Seq[Block[P, TData, CData]]

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: Block[P, TData, CData] = lastBlocks(1).head

  def lastBlocks(howMany: Int): Seq[Block[P, TData, CData]]

  def lastBlockIds(howMany: Int): Seq[BlockId] = lastBlocks(howMany).map(_.id)

  /**
    * Return howMany blocks starting from parentSignature
    */
  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId]

  /**
    * Average delay in milliseconds between last blockNum blocks starting from block
    */
  def averageDelay(blockId: BlockId, blockNum: Int): Try[Long] = Try {
    val block = blockById(blockId).get
    (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
  }

  def appendBlock(block: Block[P, TData, CData]): Try[History[P, TX, TData, CData]]

  def discardBlock(): Try[History[P, TX, TData, CData]]

  val genesisBlock: Block[P, TData, CData]
}