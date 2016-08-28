package scorex

import akka.actor.Actor
import akka.actor.Actor.Receive
import scorex.block.{Block, ConsensusData, StateChanges, TransactionalData}
import scorex.consensus.History
import scorex.transaction.{MemoryPool, Transaction}
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState
import scorex.transaction.wallet.Wallet

import scala.util.Try

trait Action

sealed trait DefaultAction extends Action

//case class NewBlock(block: Block[_, _, _]) extends DefaultAction
//case class NewOffchainTransaction(tx: Transaction[_, _]) extends DefaultAction


//todo: listeners
//todo: async update?
trait NodeStateHolder[
P <: Proposition,
TX <: Transaction[P, TX],
TData <: TransactionalData[TX],
CData <: ConsensusData] extends Actor {

  type GlobalState = (MinimalState[P, TX], History[P, TX, TData, CData], MemoryPool[TX], Wallet[P, TX])

  protected val globalState: GlobalState

  def stableState: GlobalState

  def state: MinimalState[P, TX] = stableState._1

  def history: History[P, TX, TData, CData] = stableState._2

  def mempool: MemoryPool[TX] = stableState._3

  def wallet: Wallet[P, TX] = stableState._4

  def appendBlock(block: Block[P, TData, CData], changes: StateChanges[P]): Try[GlobalState] =
    ???


  override def receive: Receive = ???

  /*{
    val curMinState = globalState._1
    val curChain = globalState._2
    val curMempool = globalState._3
    val curWallet = globalState._4

    require(curMinState.version == curChain.height())

    (curChain.appendBlock(block) flatMap { newChain =>
      curMinState.applyChanges(changes) map { newMinState =>
        val newMemPool = curMempool.filter(block.transactionalData.mbTransactions.getOrElse(Seq()))
        (newMinState, newChain, newMemPool, curWallet)
      }
    }).ensuring(_.map(gs => gs._1.version == gs._2.height()).getOrElse(true))
  }*/

  //todo: implement
  def rollBackTo(blockId: Array[Byte]): Try[GlobalState] = {
    ???
  }
}