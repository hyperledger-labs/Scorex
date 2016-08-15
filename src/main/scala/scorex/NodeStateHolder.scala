package scorex

import scorex.block.{StateChanges, Block, ConsensusData, TransactionalData}
import scorex.consensus.History
import scorex.transaction.{TransactionChanges, MemoryPool, Transaction}
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState

import scala.util.Try

//todo: notifiers
//todo: async update
//todo: wallet?
trait NodeStateHolder[
P <: Proposition,
TX <: Transaction[P, TX],
TData <: TransactionalData[TX],
CData <: ConsensusData
] {
  type GlobalState = (MinimalState[P, TX], History[P, TX, TData, CData], MemoryPool[TX])

  protected val globalState: GlobalState

  def stableState: GlobalState

  def mempool: MemoryPool[TX] = stableState._3
  def history: History[P, TX, TData, CData] = stableState._2
  def state: MinimalState[P, TX] = stableState._1

  def addOffchainTransaction(tx: TX): Try[MemoryPool[TX]] = ???

  def appendBlock(block: Block[P, TData, CData], changes: StateChanges[P]): Try[GlobalState] = {
    val curMinState = globalState._1
    val curChain = globalState._2
    val curMempool = globalState._3

    require(curMinState.version == curChain.height())

    (curChain.appendBlock(block) flatMap { newChain =>
      curMinState.applyChanges(changes) map { newMinState =>
        val newMemPool = curMempool.filter(block.transactionalData.mbTransactions.getOrElse(Seq()))
        (newMinState, newChain, newMemPool)
      }
    }).ensuring(_.map(gs => gs._1.version == gs._2.height()).getOrElse(true))
  }

  //todo: implement
  def rollBackTo(blockId: Array[Byte]): Try[GlobalState] = {
    ???
  }
}
