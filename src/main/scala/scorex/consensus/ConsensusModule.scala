package scorex.consensus

import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.crypto.encode.Base58
import scorex.serialization.BytesParseable
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.wallet.Wallet
import scorex.transaction.{Transaction, TransactionalModule}
import scorex.utils.ScorexLogging

import scala.concurrent.Future

//todo: make BytesParseable[TData] an instance also, not a mixin

trait ConsensusModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  extends ScorexLogging with BytesParseable[CData] {

  type BlockId = ConsensusData.BlockId
  val BlockIdLength: Int

  def isValid(block: Block[P, TData, CData]): Boolean

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Proof-of-Activity proposal of Bentov et al. http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block[P, TData, CData]): Map[P, Long]

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
    *
    * @param block - a block to extract producers from
    * @return blocks' producers
    */
  def producers(block: Block[P, TData, CData]): Seq[P]

  def blockScore(block: Block[P, TData, CData]): BigInt

  def generateNextBlock(wallet: Wallet[_ <: P, _ <: TransactionalModule[P, TX, TData]]): Future[Option[Block[P, TData, CData]]]

  def id(block: Block[P, TData, CData]): BlockId

  def encodedId(block: Block[P, TData, CData]): String = Base58.encode(id(block))

  def parentId(block: Block[P, TData, CData]): BlockId

  val MaxRollback: Int

  val genesisData: CData
}