package scorex.consensus

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.crypto.encode.Base58
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.{Transaction, TransactionModule}
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait ConsensusModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  extends History[P, TX, TData, CData] with ScorexLogging {

  type BlockId = ConsensusData.BlockId
  val BlockIdLength: Int

  val transactionalModule: TransactionModule[P, TX, TData]

  def isValid(block: Block[P, CData, TData]): Boolean

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Proof-of-Activity proposal of Bentov et al. http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block[P, CData, TData]): Map[P, Long]

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
    * @param block
    * @return
    */
  def producers(block: Block[P, CData, TData]): Seq[P]

  def blockScore(block: Block[P, CData, TData]): BigInt

  def generateNextBlock(): Future[Option[Block[P, CData, TData]]]

  //def generateNextBlocks(transactionModule: TransactionModule[P, _, _]): Future[Seq[B]]
  //Future.sequence(accounts.map(acc => generateNextBlock(acc))).map(_.flatten)

  def id(block: Block[P, CData, TData]): BlockId

  def encodedId(block: Block[P, CData, TData]): String = Base58.encode(id(block))

  def parentId(block: Block[P, CData, TData]): BlockId

  val MaxRollback: Int


  //Append block to current state
  //todo: check possible conflicts
  def processBlock(block: Block[P, CData, TData]): Try[Unit] = synchronized {
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
  def removeAfter(blockId: BlockId): Unit = synchronized {
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