package scorex.consensus

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.crypto.encode.Base58
import scorex.transaction.{Transaction, TransactionModule}
import scorex.transaction.box.{Proposition, PublicKey25519Proposition}
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


trait History[P <: Proposition, CData <: ConsensusData,  B <: Block[P, CData, _]] {
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


trait ConsensusModule[P <: Proposition, CData <: ConsensusData, B <: Block[P, CData, _]]
  extends History[P, CData, B] with ScorexLogging {

  type BlockId = ConsensusData.BlockId
  val BlockIdLength: Int

  def isValid(block: B)(implicit transactionModule: TransactionModule[_, _, _]): Boolean

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Proof-of-Activity proposal of Bentov et al. http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: B)(transactionModule: TransactionModule[P, _ <: Transaction[PublicKey25519Proposition, _], _]): Map[P, Long]

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
    * @param block
    * @return
    */
  def producers(block: B): Seq[P]

  def blockScore(block: B)(implicit transactionModule: TransactionModule[P, _, _]): BigInt

  def generateNextBlock[TData <: TransactionalData[_]](transactionModule: TransactionModule[P, _, TData]): Future[Option[B]]

  def generateNextBlocks[TData <: TransactionalData[_]](transactionModule: TransactionModule[P, _, TData]): Future[Seq[B]]

  //Future.sequence(accounts.map(acc => generateNextBlock(acc))).map(_.flatten)

  def id(block: B): BlockId

  def encodedId(block: B): String = Base58.encode(id(block))

  def parentId(block: B): BlockId

  def totalFee(block: B)(transactionModule: TransactionModule[P, _ <: Transaction[PublicKey25519Proposition, _], _]): Long
    = feesDistribution(block)(transactionModule).values.sum

  val MaxRollback: Int


  //Append block to current state
  //todo: check possible conflicts
  def processBlock(block: B)(implicit transactionalModule: TransactionModule[P, _, _]): Try[Unit] = synchronized {
    appendBlock(block).map { _ =>
      transactionalModule.processBlock(block) match {
        case Failure(e) =>
          log.error("Failed to apply block to state", e)
          discardBlock()
          //TODO ???
          System.exit(1)
        case Success(m) =>
      }
    }
  }

  //Should be used for linear blockchain only
  //todo: check possible conflicts
  def removeAfter(blockId: BlockId)
                 (implicit transactionalModule: TransactionModule[_, _, _]): Unit = synchronized {
    heightOf(blockId) match {
      case Some(height) =>
        while (id(lastBlock).sameElements(blockId)) {
          discardBlock()
        }
        transactionalModule.rollbackTo(height)
      case None =>
        log.warn(s"RemoveAfter non-existing block ${Base58.encode(blockId)}")
    }
  }

  val genesisData: CData
}