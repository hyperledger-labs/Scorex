package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.api.http.ApiRoute
import scorex.core.consensus.History
import scorex.core.network.{ConnectedPeer, NodeViewSynchronizer}
import scorex.core.network.NodeViewSynchronizer._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.util.{Failure, Success}


//todo: listeners
//todo: async update?

/**
  * Composite local view of the node
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * @tparam P
  * @tparam TX
  */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  extends Actor with ScorexLogging {

  import NodeViewHolder._

  type HIS <: History[P, TX, PMOD, HIS]
  type MS <: MinimalState[P, _, TX, PMOD, MS]
  type VL <: Vault[P, TX, VL]
  type MP <: MemoryPool[TX, MP]

  type NodeView = (HIS, MS, VL, MP)

  val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]]

  val networkChunkSize = 100 //todo: make configurable?


  //mutable private node view instance
  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  protected def genesisState: NodeView

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView]


  private def history(): HIS = nodeView._1

  private def minimalState(): MS = nodeView._2

  private def vault(): VL = nodeView._3

  private def memoryPool(): MP = nodeView._4

  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, Seq[ActorRef]]()

  private def notifySubscribers[O <: NodeViewHolderEvent](eventType: EventType.Value, signal: O) =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! signal)

  private def txModify(tx: TX, source: Option[ConnectedPeer]) = {
    val updWallet = vault().scan(tx, offchain = true)
    memoryPool().put(tx) match {
      case Success(updPool) =>
        nodeView = (history(), minimalState(), updWallet, updPool)
        notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx, source))

      case Failure(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e, source))
    }
  }

  private def pmodModify(pmod: PMOD, source: Option[ConnectedPeer]) = {
    notifySubscribers(
      EventType.StartingPersistentModifierApplication,
      StartingPersistentModifierApplication[P, TX, PMOD](pmod)
    )

    history().append(pmod) match {
      case Success((newHistory, maybeRollback)) =>
        maybeRollback.map(rb => minimalState().rollbackTo(rb.to).flatMap(_.applyModifiers(rb.applied)))
          .getOrElse(Success(minimalState()))
          .flatMap(minState => minState.applyModifier(pmod)) match {

          case Success(newMinState) =>
            val rolledBackTxs = maybeRollback.map(rb => rb.thrown.flatMap(_.transactions).flatten).getOrElse(Seq())

            val appliedTxs = maybeRollback.map(rb =>
              rb.applied.flatMap(_.transactions).flatten).getOrElse(Seq()
            ) ++ pmod.transactions.getOrElse(Seq())

            val newMemPool = memoryPool().putWithoutCheck(rolledBackTxs).filter(appliedTxs)

            val newWallet = maybeRollback
              .map(rb => vault().rollback(rb.to).get) //we consider that vault always able to perform a rollback needed
              .map(w => w.bulkScan(appliedTxs, offchain = false))
              .getOrElse(vault())

            nodeView = (newHistory, newMinState, newWallet, newMemPool)
            notifySubscribers(EventType.SuccessfulPersistentModifier, SuccessfulModification[P, TX, PMOD](pmod, source))

          case Failure(e) =>
            log.warn(s"Cant' apply persistent modifier (id: ${pmod.id}, contents: $pmod) to minimal state")
            notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
        }

      case Failure(e) =>
        log.warn(s"Cant' apply persistent modifier (id: ${pmod.id}, contents: $pmod) to history")
        notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
    }
  }

  private def modify[MOD <: NodeViewModifier](m: MOD, source: Option[ConnectedPeer]): Unit = m match {
    case (tx: TX@unchecked) if m.modifierTypeId == Transaction.TransactionModifierId =>
      txModify(tx, source)

    case pmod: PMOD@unchecked =>
      pmodModify(pmod, source)
  }

  def apis: Seq[ApiRoute] = Seq(
    genesisState._1,
    genesisState._2,
    genesisState._3,
    genesisState._4
  ).map(_.companion.api)

  private def handleSubscribe: Receive = {
    case NodeViewHolder.Subscribe(events) =>
      events.foreach { evt =>
        val current = subscribers.getOrElse(evt, Seq())
        subscribers.put(evt, current :+ sender())
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
          modifierIds.flatMap(id => history().blockById(id)) //todo: why block by id?
      }
      sender() ! NodeViewSynchronizer.ResponseFromLocal(sid, modifierTypeId, objs)
  }

  private def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierCompanions.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parse(r).toOption).foreach(m =>
          modify(m, Some(remote))
        )
      }
  }

  private def processLocallyGeneratedModifiers: Receive = {
    case lt: LocallyGeneratedTransaction[P, TX] =>
      modify(lt.tx, None)

    case lm: LocallyGeneratedModifier[P, TX, PMOD] =>
      modify(lm.pmod, None)
  }

  override def receive: Receive =
    handleSubscribe orElse
      compareViews orElse
      readLocalObjects orElse
      processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse {
      case GetCurrentView =>
        sender() ! (history(), minimalState(), vault(), memoryPool())
      case a: Any => log.error("Strange input: " + a)
    }
}


object NodeViewHolder {

  object GetCurrentView

  object EventType extends Enumeration {
    val FailedTransaction = Value(1)
    val FailedPersistentModifier = Value(2)
    val SuccessfulTransaction = Value(3)
    val SuccessfulPersistentModifier = Value(4)

    val StartingPersistentModifierApplication = Value(5)
  }

  //a command to subscribe for events
  case class Subscribe(events: Seq[EventType.Value])

  trait NodeViewHolderEvent

  //node view holder starting persistent modifier application
  case class StartingPersistentModifierApplication[
  P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]
  ](modifier: PMOD) extends NodeViewHolderEvent

  //hierarchy of events regarding modifiers application outcome
  trait ModificationOutcome extends NodeViewHolderEvent {
    val source: Option[ConnectedPeer]
  }

  case class FailedTransaction[P <: Proposition, TX <: Transaction[P]]
  (transaction: TX, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class FailedModification[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (modifier: PMOD, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]]
  (transaction: TX, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class SuccessfulModification[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier[P, TX]]
  (modifier: PMOD, override val source: Option[ConnectedPeer]) extends ModificationOutcome

}