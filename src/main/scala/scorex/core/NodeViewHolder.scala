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
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P, TX], PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  type HIS <: History[P, TX, PMOD]
  type MS <: MinimalState[P, TX, PMOD]
  type WL <: Wallet[P, TX]
  type MP <: MemoryPool[TX]



  type NodeView = (HIS, MS, WL, MP)

  val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]]

  val networkChunkSize = 100 //todo: fix

  def restoreState(): Option[NodeView]

  private var nodeState: NodeView = restoreState().getOrElse(genesisState)

  private def history(): HIS = nodeState._1

  private def minimalState(): MS = nodeState._2

  private def wallet(): WL = nodeState._3

  private def memoryPool(): MP = nodeState._4

  lazy val historyCompanion = history().companion

  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, ActorRef]()

  //todo: ???
  def fixDb()

  def modify[MOD <: NodeViewModifier](m: MOD) = {

    /*
    val modification = historyCompanion.produceModification(history(), m)
    val hisModDone = modification.process()
    val md = hisModDone.join[MS](minimalState())
    val wld = md.join[WL](wallet())
    val mpd = wld.join[MP](memoryPool())*/

    fixDb()

    m match {
      case tx: TX =>
        val updWallet  = wallet().scan(tx)
        memoryPool().put(tx) match {
          case Success(updPool) =>

          case Failure(e) =>

        }

      case pmod: PMOD =>
        history().append(pmod) match {
          case Success((newHis, maybeRb)) =>
            val a = maybeRb.map(rb => minimalState().rollbackTo(rb.to))
              .getOrElse(Success(minimalState()))
              .flatMap(minState => minState.applyChanges(pmod))
            a
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

  //case class UpdateOutcome[M <: NodeViewModifier](eventType: EventType.Value, modifier: M)

  case class Subscribe(events: Seq[EventType.Value])
}