package scorex.consensus

import scorex.block.{Block, ConsensusData}
import scorex.crypto.encode.Base58
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.wallet.Wallet
import scorex.utils.ScorexLogging

import scala.concurrent.Future

trait ConsensusModule[P <: Proposition, CData <: ConsensusData] extends ScorexLogging {

  type BlockId = ConsensusData.BlockId
  val BlockIdLength: Int

  def isValid(cdata: CData): Boolean

  def isValid(block: Block[P, _, CData]): Boolean = isValid(block.consensusData)

  /**
   * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
   * Proof-of-Activity proposal of Bentov et al. http://eprint.iacr.org/2014/452.pdf
   */
  def feesDistribution(cdata: CData, txSumFee: Long): Map[P, Long]

  /**
   * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
   * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
   *
   * @return blocks' producers
   */
  def producers(cdata: CData): Seq[P]

  def producers(block: Block[P, _, CData]): Seq[P] = producers(block.consensusData)

  def blockScore(cdata: CData): BigInt

  def blockScore(block: Block[P, _, CData]): BigInt = blockScore(block.consensusData)

  def generateCdata(wallet: Wallet[_ <: P, _], time: Long, txsId: Array[Byte]): Future[Option[CData]]

  def parentId(cdata: CData): BlockId

  def parentId(b: Block[P, _, CData]): BlockId = parentId(b.consensusData)

  val MaxRollback: Int

  val genesisData: CData
}