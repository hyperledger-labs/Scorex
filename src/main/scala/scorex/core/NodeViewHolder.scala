package scorex.core

import akka.actor.{Actor, ActorRef}
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.consensus.History.{HistoryComparisonResult, ProgressInfo}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NodeViewSynchronizer._
import scorex.core.network.{ConnectedPeer, NodeViewSynchronizer}
import scorex.core.serialization.Serializer
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{MinimalState, TransactionValidation}
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}


/**
  * Composite local view of the node
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * @tparam P
  * @tparam TX
  * @tparam PMOD
  */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  import NodeViewHolder._

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[P, TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]


  type NodeView = (HIS, MS, VL, MP)
  /**
    * The main datastructure a node software is taking care about, a node view consists
    * of four elements to be updated atomically: history (log of persistent modifiers),
    * state (result of log's modifiers application to pre-historical(genesis) state,
    * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
    */
  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView]

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  protected def genesisState: NodeView


  private def history(): HIS = nodeView._1

  private def minimalState(): MS = nodeView._2

  private def vault(): VL = nodeView._3

  private def memoryPool(): MP = nodeView._4


  /**
    * Serializers for modifiers, to be provided by a concrete instantiation
    */
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]]

  //todo: write desc
  /**
    *
    */
  val networkChunkSize: Int


  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  //todo: make configurable limited size
  private val modifiersCache = mutable.Map[ModifierId, (ConnectedPeer, PMOD)]()


  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, Seq[ActorRef]]()

  private def notifySubscribers[O <: NodeViewHolderEvent](eventType: EventType.Value, signal: O) =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! signal)

  private def txModify(tx: TX, source: Option[ConnectedPeer]) = {
    //todo: async update?
    val errorOpt: Option[Exception] = minimalState() match {
      case txValidator: TransactionValidation[P, TX] =>
        txValidator.validate(tx) match {
          case Success(_) => None
          case Failure(e) => Some(e)
        }
      case _ => None
    }

    errorOpt match {
      case None =>
        memoryPool().put(tx) match {
          case Success(updPool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            val updWallet = vault().scanOffchain(tx)
            nodeView = (history(), minimalState(), updWallet, updPool)
            notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx, source))

          case Failure(e) =>
            notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e, source))
        }

      case Some(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e, source))
    }
  }

  protected def extractTransactions(mods: Seq[PMOD]): Seq[TX] = {
    mods.flatMap { mod =>
      mod match {
        case tcm: TransactionsCarryingPersistentNodeViewModifier[P, TX] => tcm.transactions
        case _ => Seq()
      }
    }
  }

  //todo: this method caused delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  protected def updateMemPool(progressInfo: History.ProgressInfo[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs = extractTransactions(progressInfo.toRemove)

    val appliedTxs = extractTransactions(progressInfo.toApply)

    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[P, TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  /*

    Assume history knew following blocktree:

           G
          / \
         *   G
        /     \
       *       G

    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain (* means
    that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to the root +
    application of the sidechain to the state. Assume that state is finding that some modifier in the sidechain is
    incorrect:

           G
          / \
         G   G
        /     \
       B       G
      /
     *

    In this case history should be informed about the bad modifier and it should retarget state

    todo: write tests for this case
       */

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD]
                         ): (HIS, Try[MS]) = {
    val stateToApplyTry = if (progressInfo.chainSwitchingNeeded) {
      val branchingPoint = VersionTag @@ progressInfo.branchPoint.get
      if (!state.version.sameElements(branchingPoint)) state.rollbackTo(branchingPoint) else Success(state)
    } else Success(state)

    stateToApplyTry match {
      case Success(stateToApply) =>
        progressInfo.toApply.headOption match {
          case Some(modToApply) =>
            stateToApply.applyModifier(modToApply) match {
              case Success(stateAfterApply) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = true, modToApply.id)
                updateState(newHis, stateAfterApply, newProgressInfo)
              case Failure(e) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = false, ModifierId @@ state.version)
                //todo: send signal
                updateState(newHis, stateToApply, newProgressInfo)
            }

          case None =>
            history -> Success(stateToApply)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        notifySubscribers[RollbackFailed.type](EventType.FailedRollback, RollbackFailed)
        //todo: what to return here?
        ???
    }
  }

  //todo: update state in async way?
  private def pmodModify(pmod: PMOD, source: Option[ConnectedPeer]): Unit =
    if (!history().contains(pmod.id)) {
      notifySubscribers(
        EventType.StartingPersistentModifierApplication,
        StartingPersistentModifierApplication[PMOD](pmod)
      )

      log.info(s"Apply modifier to nodeViewHolder: ${Base58.encode(pmod.id)}")

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications: $progressInfo")

          if (progressInfo.toApply.nonEmpty) {
            val (newHistory, newStateTry) = updateState(historyBeforeStUpdate, minimalState(), progressInfo)

            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo, memoryPool(), newMinState)

                //we consider that vault always able to perform a rollback needed
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get.scanPersistent(progressInfo.toApply)
                } else {
                  vault().scanPersistent(progressInfo.toApply)
                }

                log.info(s"Persistent modifier ${Base58.encode(pmod.id)} applied successfully")
                nodeView = (newHistory, newMinState, newVault, newMemPool)
                notifySubscribers(EventType.SuccessfulPersistentModifier, SuccessfulModification[PMOD](pmod, source))

              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${Base58.encode(pmod.id)}, contents: $pmod) to minimal state", e)
                nodeView = (newHistory, minimalState(), vault(), memoryPool())
                notifySubscribers(EventType.FailedPersistentModifier, FailedModification[PMOD](pmod, e, source))
            }
          } else {
            //todo: send signal?
            nodeView = (historyBeforeStUpdate, minimalState(), vault(), memoryPool())
          }
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${Base58.encode(pmod.id)}, contents: $pmod) to history", e)
          notifySubscribers(EventType.FailedPersistentModifier, FailedModification[PMOD](pmod, e, source))
      }
    } else {
      log.warn(s"Trying to apply modifier ${Base58.encode(pmod.id)} that's already in history")
    }


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
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(mid))
      }

      sender() ! NodeViewSynchronizer.RequestFromLocal(sid, modifierTypeId, ids)
  }


  private def readLocalObjects: Receive = {
    case GetLocalObjects(sid, modifierTypeId, modifierIds) =>
      val objs: Seq[NodeViewModifier] = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          memoryPool().getAll(modifierIds)
        case typeId: ModifierTypeId =>
          modifierIds.flatMap(id => history().modifierById(id))
      }

      log.debug(s"Requested modifiers ${modifierIds.map(Base58.encode)}, sending: " + objs.map(_.id).map(Base58.encode))
      sender() ! NodeViewSynchronizer.ResponseFromLocal(sid, modifierTypeId, objs)
  }

  private def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case (tx: TX@unchecked) if tx.modifierTypeId == Transaction.ModifierTypeId =>
            txModify(tx, Some(remote))

          case pmod: PMOD@unchecked =>
            modifiersCache.put(pmod.id, remote -> pmod)
        }

        log.debug(s"Cache before(${modifiersCache.size}): ${modifiersCache.keySet.map(Base58.encode).mkString(",")}")

        var t: Option[(ConnectedPeer, PMOD)] = None
        do {
          t = {
            modifiersCache.find { case (_, (_, pmod)) =>
              history().applicable(pmod)
            }.map { t =>
              val res = t._2
              modifiersCache.remove(t._1)
              res
            }
          }
          t.foreach { case (peer, pmod) => pmodModify(pmod, Some(peer)) }
        } while (t.isDefined)

        log.debug(s"Cache after(${modifiersCache.size}): ${modifiersCache.keySet.map(Base58.encode).mkString(",")}")
      }
  }

  private def processLocallyGeneratedModifiers: Receive = {
    case lt: LocallyGeneratedTransaction[P, TX] =>
      txModify(lt.tx, None)

    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier: ${Base58.encode(lm.pmod.id)}")
      pmodModify(lm.pmod, None)
  }

  private def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  private def compareSyncInfo: Receive = {
    case OtherNodeSyncingInfo(remote, syncInfo: SI) =>
      log.debug(s"Comparing remote info having starting points: ${syncInfo.startingPoints.map(_._2).map(Base58.encode)}")
      syncInfo.startingPoints.map(_._2).headOption.foreach { head =>
        log.debug(s"Local side contains head: $head")
      }

      val extensionOpt = history().continuationIds(syncInfo, networkChunkSize)
      val ext = extensionOpt.getOrElse(Seq())
      val comparison = history().compare(syncInfo)
      log.debug(s"Sending extension of length ${ext.length}: ${ext.map(_._2).map(Base58.encode).mkString(",")}")
      log.debug("Comparison result is: " + comparison)

      if (!(extensionOpt.nonEmpty || comparison != HistoryComparisonResult.Younger)) {
        log.warn("Extension is empty while comparison is younger")
      }

      sender() ! OtherNodeSyncingStatus(
        remote,
        comparison,
        syncInfo,
        history().syncInfo(true),
        extensionOpt
      )
  }

  private def getSyncInfo: Receive = {
    case GetSyncInfo =>
      sender() ! CurrentSyncInfo(history().syncInfo(false))
  }

  override def receive: Receive =
    handleSubscribe orElse
      compareViews orElse
      readLocalObjects orElse
      processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      getCurrentInfo orElse
      getSyncInfo orElse
      compareSyncInfo orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}


object NodeViewHolder {

  case object GetSyncInfo

  case class CurrentSyncInfo[SI <: SyncInfo](syncInfo: SyncInfo)

  case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

  object EventType extends Enumeration {
    //finished modifier application, successful of failed
    val FailedTransaction = Value(1)
    val FailedPersistentModifier = Value(2)
    val SuccessfulTransaction = Value(3)
    val SuccessfulPersistentModifier = Value(4)

    //starting persistent modifier application. The application could be slow
    val StartingPersistentModifierApplication = Value(5)

    //rollback failed, really wrong situation, probably
    val FailedRollback = Value(6)
  }

  //a command to subscribe for events
  case class Subscribe(events: Seq[EventType.Value])

  trait NodeViewHolderEvent

  case class OtherNodeSyncingStatus[SI <: SyncInfo](peer: ConnectedPeer,
                                                    status: History.HistoryComparisonResult.Value,
                                                    remoteSyncInfo: SI,
                                                    localSyncInfo: SI,
                                                    extension: Option[Seq[(ModifierTypeId, ModifierId)]])

  //node view holder starting persistent modifier application
  case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

  //hierarchy of events regarding modifiers application outcome
  trait ModificationOutcome extends NodeViewHolderEvent {
    val source: Option[ConnectedPeer]
  }

  case class FailedTransaction[P <: Proposition, TX <: Transaction[P]]
  (transaction: TX, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class FailedModification[PMOD <: PersistentNodeViewModifier]
  (modifier: PMOD, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]]
  (transaction: TX, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class SuccessfulModification[PMOD <: PersistentNodeViewModifier]
  (modifier: PMOD, override val source: Option[ConnectedPeer]) extends ModificationOutcome

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  //todo: consider sending info on the rollback
  case object RollbackFailed extends NodeViewHolderEvent
}