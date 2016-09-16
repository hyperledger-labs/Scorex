package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.network.{ConnectedPeer, NodeViewSynchronizer}
import scorex.core.network.NodeViewSynchronizer._
import scorex.core.transaction.NodeViewModifier.ModifierTypeId
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Wallet
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.util.{Failure, Success}


//todo: listeners
//todo: async update?

/**
  * Composite local view
  *
  * @tparam P
  * @tparam TX
  */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P, TX], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor with ScorexLogging {

  import NodeViewHolder._

  type HIS <: History[P, TX, PMOD, HIS]
  type MS <: MinimalState[P, TX, PMOD, MS]
  type WL <: Wallet[P, TX, WL]
  type MP <: MemoryPool[TX, MP]


  type NodeView = (HIS, MS, WL, MP)

  val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]]

  val networkChunkSize = 100 //todo: fix

  def restoreState(): Option[NodeView]

  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  private def history(): HIS = nodeView._1

  private def minimalState(): MS = nodeView._2

  private def wallet(): WL = nodeView._3

  private def memoryPool(): MP = nodeView._4

  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, ActorRef]()

  //todo: "some check and fix database" function
  def fixDb()

  private def notifySubscribers[O <: ModificationOutcome](eventType: EventType.Value, signal: O) = subscribers.get(eventType).foreach(_ ! signal)

  private def txModify(tx: TX, source: Option[ConnectedPeer]) = {
    val updWallet = wallet().scan(tx)
    memoryPool().put(tx) match {
      case Success(updPool) =>
        nodeView = (history(), minimalState(), updWallet, updPool)
        notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx, source))

      case Failure(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e, source))
    }
  }

  private def pmodModify(pmod: PMOD, source: Option[ConnectedPeer]) = {
    history().append(pmod) match {
      case Success((newHistory, maybeRollback)) =>
        maybeRollback.map(rb => minimalState().rollbackTo(rb.to).flatMap(_.applyChanges(rb.applied)))
          .getOrElse(Success(minimalState()))
          .flatMap(minState => minState.applyChanges(pmod)) match {

          case Success(newMinState) =>
            val rolledBackTxs = maybeRollback.map(rb => rb.thrown.flatMap(_.transactions).flatten).getOrElse(Seq())

            val appliedTxs = maybeRollback.map(rb =>
              rb.applied.flatMap(_.transactions).flatten).getOrElse(Seq()
            ) ++ pmod.transactions.getOrElse(Seq())

            val newMemPool = memoryPool().putWithoutCheck(rolledBackTxs).filter(appliedTxs)

            //todo: continue from here
            maybeRollback.map(rb => wallet().rollback(rb.to))
              .getOrElse(Success(wallet()))
              .map(w => w.bulkScan(appliedTxs)) match {

              case Success(newWallet) =>
                nodeView = (newHistory, newMinState, newWallet, newMemPool)

              case Failure(e) =>
                notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
            }

          case Failure(e) =>
            notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
        }

      case Failure(e) =>
        notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
    }
  }

  private def modify[MOD <: NodeViewModifier](m: MOD, source: Option[ConnectedPeer]): Unit = {
    m match {
      case (tx: TX@unchecked) if m.modifierTypeId == Transaction.TransactionModifierId =>
        txModify(tx, source)

      case pmod: PMOD@unchecked =>
        pmodModify(pmod, source)
    }
    fixDb()
  }

  protected def genesisState: NodeView

  def apis: Seq[ApiRoute] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  ).map(_.companion.api)

  private def handleSubscribe: Receive = {
    case NodeViewHolder.Subscribe(events) =>
      events.foreach(evt => subscribers.put(evt, sender()))
  }

  private def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(sid, modifierTypeId, remoteObjects, remote) =>
      modifierCompanions.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parse(r).toOption).foreach(m =>
          modify(m, Some(remote))
        )
      }
  }

  private def compareViews: Receive = {
    case CompareViews(sid, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          history().continuationIds(modifierIds, networkChunkSize)
      }

      sender() ! NodeViewSynchronizer.RequestFromLocal(sid, modifierTypeId, ids)
  }

  private def readLocalObjects: Receive = {
    case GetLocalObjects(sid, modifierTypeId, modifierIds) =>
      val objs: Seq[NodeViewModifier] = modifierTypeId match {
        case typeId: Byte if typeId == Transaction.TransactionModifierId =>
          memoryPool().getAll(modifierIds)
        case typeId: Byte =>
          modifierIds.flatMap(id => history().blockById(id))
      }
      sender() ! NodeViewSynchronizer.ResponseFromLocal(sid, modifierTypeId, objs)
  }

  override def receive: Receive =
    handleSubscribe orElse
      compareViews orElse
      readLocalObjects orElse
      processRemoteModifiers
}


object NodeViewHolder {

  object EventType extends Enumeration {
    val FailedTransaction = Value(1)
    val FailedPersistentModifier = Value(2)
    val SuccessfulTransaction = Value(3)
    val SuccessfulPersistentModifier = Value(4)
  }

  trait ModificationOutcome {
    val source: Option[ConnectedPeer]
  }

  case class FailedTransaction[P <: Proposition, TX <: Transaction[P, TX]]
  (transaction: TX, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class FailedModification[P <: Proposition, TX <: Transaction[P, TX], PMOD <: PersistentNodeViewModifier[P, TX]](modifier: PMOD, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P, TX]](transaction: TX, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  //todo: successful modification

  case class Subscribe(events: Seq[EventType.Value])

}