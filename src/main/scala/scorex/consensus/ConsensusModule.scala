package scorex.consensus

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.crypto.encode.Base58
import scorex.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.transaction.{Transaction, TransactionModule}
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

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