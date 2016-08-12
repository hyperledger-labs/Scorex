package scorex.consensus

import scorex.block.{Block, ConsensusData}
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
   * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
   * (see e.g. Proof-of-Activity paper of Bentov et al. http://eprint.iacr.org/2014/452.pdf)
   *
   * @return blocks' producers
   */
  def producers(cdata: CData): Seq[P]

  def producers(block: Block[P, _, CData]): Seq[P] = producers(block.consensusData)

  def generateCdata(wallet: Wallet[_ <: P, _], time: Long, txsId: Array[Byte]): Future[Option[CData]]

  val MaxRollback: Int

  val genesisData: CData
}