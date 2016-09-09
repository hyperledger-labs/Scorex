package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.NodeViewSynchronizer._
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Wallet
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.util.{Failure, Success}

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent

  def companion: NodeViewComponentCompanion
}

trait NodeViewComponentCompanion {

  def api: ApiRoute

  //def produceModification[M <: NodeStateModifier, CompType <: NodeViewComponent](component: CompType, m: M): UndoneModification[M, CompType]

  //network functions to call
}


//todo: listeners
//todo: async update?

/**
  * Composite local view
  *
  * @tparam P
  * @tparam TX
  */
/*
 (S, H, MP, W)
 - process state modifier M in following order (H -> S -> MP -> W)

 - HxM -> Outcome[H]
 */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P, TX], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor with ScorexLogging {

  import NodeViewHolder._

  type HIS <: History[P, TX, PMOD]
  type MS <: MinimalState[P, TX, PMOD]
  type WL <: Wallet[P, TX]
  type MP <: MemoryPool[TX]


  type NodeView = (HIS, MS, WL, MP)

  val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]]

  val networkChunkSize = 100 //todo: fix

  def restoreState(): Option[NodeView]

  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  private def history(): HIS = nodeView._1

  private def minimalState(): MS = nodeView._2

  private def wallet(): WL = nodeView._3

  private def memoryPool(): MP = nodeView._4

  lazy val historyCompanion = history().companion

  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, ActorRef]()

  //todo: ???
  def fixDb()

  //todo: fix Any
  def notifySubscribers(eventType: EventType.Value, signal: Any) = subscribers.get(eventType).foreach(_ ! signal)

  def modify[MOD <: NodeViewModifier](m: MOD) = {

    fixDb()

    m match {
      case tx: TX =>
        val updWallet = wallet().scan(tx)
        memoryPool().put(tx) match {
          case Success(updPool) =>
            notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx))

          //todo: uncomment & fix types  |||  nodeView = (history(), minimalState(), updWallet, updPool)

          case Failure(e) =>
            notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e))
        }

      case pmod: PMOD =>
        history().append(pmod) match {
          case Success((newHis, maybeRb)) =>
            maybeRb.map(rb => minimalState().rollbackTo(rb.to))
              .getOrElse(Success(minimalState()))
              .flatMap(minState => minState.applyChanges(pmod)) match {

              case Success(newMinState) =>
                val txsToAdd = maybeRb.map(rb => rb.thrown.flatMap(_.transactions).flatten).getOrElse(Seq())
                val txsToRemove = pmod.transactions.getOrElse(Seq())

                val newMemPool = memoryPool().putWithoutCheck(txsToAdd).filter(txsToRemove)

                //todo: continue from here
                maybeRb.map(rb => wallet().rollback(rb.to))
                  .getOrElse(Success(wallet()))
                  .map(w => w.bulkScan(txsToAdd)) match {

                  case Success(newWallet) =>
                  //todo: uncomment & fix types  ||| nodeView = (newHis, newMinState, newWallet, newMemPool)

                  case Failure(e) =>

                }

              case Failure(e) =>

            }

          case Failure(e) =>
        }


      case a: Any => log.error(s"Wrong kind of modifier: $a")
    }
  }

  protected def genesisState: NodeView

  def apis: Seq[ApiRoute] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  ).map(_.companion.api)

  override def receive: Receive =
    subscribe orElse
      compareViews orElse
      readLocalObjects orElse
      processRemoteObjects

  def subscribe: Receive = {
    case NodeViewHolder.Subscribe(events) =>
      events.foreach(evt => subscribers.put(evt, sender()))
  }

  def processRemoteObjects: Receive = {
    case ModifiersFromRemote(sid, modifierTypeId, remoteObjects) =>
      modifierCompanions.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parse(r).toOption) foreach modify
      }
  }

  def compareViews: Receive = {
    case CompareViews(sid, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          memoryPool().notIn(modifierIds)
        case typeId: Byte =>
          history().continuationIds(modifierIds, networkChunkSize)
      }

      sender() ! NodeViewSynchronizer.RequestFromLocal(sid, modifierTypeId, ids)
  }

  def readLocalObjects: Receive = {
    case GetLocalObjects(sid, modifierTypeId, modifierIds) =>
      val objs: Seq[NodeViewModifier] = modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          memoryPool().getAll(modifierIds)
        case typeId: Byte =>
          modifierIds.flatMap(id => history().blockById(id))
      }
      sender() ! NodeViewSynchronizer.ResponseFromLocal(sid, modifierTypeId, objs)
  }
}


object NodeViewHolder {

  object EventType extends Enumeration {
    val FailedTransaction = Value(1)
    val FailedPersistentModifier = Value(2)
    val SuccessfulTransaction = Value(3)
    val SuccessfulPersistentModifier = Value(4)
  }

  case class FailedTransaction[P <: Proposition, TX <: Transaction[P, TX]](transaction: TX, error: Throwable)

  case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P, TX]](transaction: TX)

  case class Subscribe(events: Seq[EventType.Value])

}