package scorex.consensus

import scorex.block.{Block, ConsensusData}
import scorex.crypto.encode.Base58
import scorex.transaction.box.proposition.Proposition

import scala.util.Try

trait History[P <: Proposition, CData <: ConsensusData, B <: Block[P, CData, _]] {
  this: ConsensusModule[P, CData, B] =>

  //todo: finish
  //val id: BlockId
  //val parentId: BlockId
  //val producerField: ProducerBlockField
  //val signatureField: SignatureBlockField
  //lazy val history = transactionModule.blockStorage.history.contains(referenceField.value)
  //lazy val sigValid = producer.verify(messageToSign, signature, nonce)

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

  /**
    * Height of the a chain, or a longest chain in the explicit block-tree
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

  def contains(block: B): Boolean = contains(id(block))

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: BlockId): Option[B]

  def blockById(blockId: String): Option[B] =
    Base58
      .decode(blockId)
      .toOption
      .flatMap(blockById)

  /**
    * Height of a block if it's in the blocktree
    */
  def heightOf(block: B): Option[Int] = heightOf(id(block))

  def heightOf(blockId: BlockId): Option[Int]

  def parent(block: B, back: Int = 1): Option[B]

  def confirmations(block: B): Option[Int] = heightOf(block).map(height() - _)

  def generatedBy(id: P): Seq[B]

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: B = lastBlocks(1).head

  def lastBlocks(howMany: Int): Seq[B]

  def lastBlockIds(howMany: Int): Seq[BlockId] = lastBlocks(howMany).map(id)

  /**
    * Return $howMany blocks starting from $parentSignature
    */
  def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId]

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    */
  def averageDelay(block: B, blockNum: Int): Try[Long] = Try {
    (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
  }

  def appendBlock(block: B): Try[Unit]

  def discardBlock(): Try[Unit]

  val genesisBlock: B
}
