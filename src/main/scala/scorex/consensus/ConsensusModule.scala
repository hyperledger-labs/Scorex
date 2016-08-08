package scorex.consensus

import scorex.block.ConsensusData
import scorex.crypto.encode.Base58
import scorex.serialization.BytesParseable
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.wallet.Wallet
import scorex.utils.ScorexLogging

import scala.concurrent.Future

trait ConsensusModule[P <: Proposition, CData <: ConsensusData] extends ScorexLogging {

  type BlockId = ConsensusData.BlockId
  val BlockIdLength: Int

  def isValid(cdata: CData): Boolean

  /**
   * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
   * Proof-of-Activity proposal of Bentov et al. http://eprint.iacr.org/2014/452.pdf
   */
  def feesDistribution(cdata: CData, txSumFee: Long): Map[P, Long]

  /**
   * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
   * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
   *
   * @param block - a block to extract producers from
   * @return blocks' producers
   */
  def producers(block: CData): Seq[P]

  def blockScore(cdata: CData): BigInt

  def generateNextCdata(wallet: Wallet[_ <: P, _]): Future[Option[CData]]

  def id(cdata: CData): BlockId

  def encodedId(cdata: CData): String = Base58.encode(id(cdata))

  def parentId(cdata: CData): BlockId

  val MaxRollback: Int

  val genesisData: CData
}