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
    * The main data structure a node software is taking care about, a node view consists
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


  protected def history(): HIS = nodeView._1

  protected def minimalState(): MS = nodeView._2

  protected def vault(): VL = nodeView._3

  protected def memoryPool(): MP = nodeView._4


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
  private val modifiersCache = mutable.Map[ModifierId, PMOD]()


  private val subscribers = mutable.Map[NodeViewHolder.EventType.Value, Seq[ActorRef]]()

  protected def notifySubscribers[O <: NodeViewHolderEvent](eventType: EventType.Value, signal: O) =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! signal)

  protected def txModify(tx: TX) = {
    //todo: async validation?
    val errorOpt: Option[Throwable] = minimalState() match {
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
            notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx))

          case Failure(e) =>
            notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e))
        }

      case Some(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e))
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

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
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

  private def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      notifySubscribers(EventType.DownloadNeeded, DownloadRequest(tid, id))
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
                          progressInfo: ProgressInfo[PMOD]): (HIS, Try[MS]) = {
    requestDownloads(progressInfo)

    val stateToApplyTry: Try[MS] = if (progressInfo.chainSwitchingNeeded) {
      Try {
        val branchingPoint = VersionTag @@ progressInfo.branchPoint.get

        if (!state.version.sameElements(branchingPoint)) {
          state.rollbackTo(branchingPoint).map { rs =>
            notifySubscribers[ChangedState](EventType.StateChanged, ChangedState(isRollback = true, rs.version))
            rs
          }
        } else Success(state)
      }.flatten
    } else Success(state)

    stateToApplyTry match {
      case Success(stateToApply) =>
        progressInfo.toApply.headOption match {
          case Some(modToApply) =>
            stateToApply.applyModifier(modToApply) match {
              case Success(stateAfterApply) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = true, modToApply.id)
                notifySubscribers[SemanticallySuccessfulModifier[PMOD]](EventType.SuccessfulSemanticallyValidModifier, SemanticallySuccessfulModifier(modToApply))
                notifySubscribers[ChangedState](EventType.StateChanged, ChangedState(isRollback = false, stateToApply.version))
                updateState(newHis, stateAfterApply, newProgressInfo)
              case Failure(e) =>
                val (newHis, newProgressInfo) = history.reportSemanticValidity(modToApply, valid = false, ModifierId @@ state.version)
                notifySubscribers[SemanticallyFailedModification[PMOD]](EventType.SemanticallyFailedPersistentModifier, SemanticallyFailedModification(modToApply, e))
                updateState(newHis, stateToApply, newProgressInfo)
            }

          case None =>
            history -> Success(stateToApply)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        notifySubscribers[RollbackFailed.type](EventType.FailedRollback, RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  //todo: update state in async way?
  protected def pmodModify(pmod: PMOD): Unit =
    if (!history().contains(pmod.id)) {
      notifySubscribers(EventType.StartingPersistentModifierApplication, StartingPersistentModifierApplication(pmod))

      log.info(s"Apply modifier to nodeViewHolder: ${Base58.encode(pmod.id)}")

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications to the state: $progressInfo")
          notifySubscribers(EventType.SuccessfulSyntacticallyValidModifier, SyntacticallySuccessfulModifier(pmod))
          notifySubscribers(EventType.OpenSurfaceChanged, NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

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

              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${Base58.encode(pmod.id)}, contents: $pmod) to minimal state", e)
                nodeView = (newHistory, minimalState(), vault(), memoryPool())
                notifySubscribers(EventType.SyntacticallyFailedPersistentModifier, SyntacticallyFailedModification(pmod, e))
            }
          } else {
            requestDownloads(progressInfo)
            nodeView = (historyBeforeStUpdate, minimalState(), vault(), memoryPool())
          }
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${Base58.encode(pmod.id)}, contents: $pmod) to history", e)
          notifySubscribers(EventType.SyntacticallyFailedPersistentModifier, SyntacticallyFailedModification(pmod, e))
      }
    } else {
      log.warn(s"Trying to apply modifier ${Base58.encode(pmod.id)} that's already in history")
    }


  protected def handleSubscribe: Receive = {
    case NodeViewHolder.Subscribe(events) =>
      events.foreach { evt =>
        val current = subscribers.getOrElse(evt, Seq())
        subscribers.put(evt, current :+ sender())
      }
  }


  protected def compareViews: Receive = {
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(mid))
      }

      sender() ! NodeViewSynchronizer.RequestFromLocal(peer, modifierTypeId, ids)
  }


  protected def readLocalObjects: Receive = {
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

  protected def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case (tx: TX@unchecked) if tx.modifierTypeId == Transaction.ModifierTypeId =>
            txModify(tx)

          case pmod: PMOD@unchecked =>
            modifiersCache.put(pmod.id, pmod)
        }

        log.debug(s"Cache before(${modifiersCache.size}): ${modifiersCache.keySet.map(Base58.encode).mkString(",")}")

        var t: Option[PMOD] = None
        do {
          t = {
            modifiersCache.find { case (_, pmod) =>
              history().applicable(pmod)
            }.map { t =>
              val res = t._2
              modifiersCache.remove(t._1)
              res
            }
          }
          t.foreach(pmodModify)
        } while (t.isDefined)

        log.debug(s"Cache after(${modifiersCache.size}): ${modifiersCache.keySet.map(Base58.encode).mkString(",")}")
      }
  }

  protected def processLocallyGeneratedModifiers: Receive = {
    case lt: LocallyGeneratedTransaction[P, TX] =>
      txModify(lt.tx)

    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier: ${Base58.encode(lm.pmod.id)}")
      pmodModify(lm.pmod)
  }

  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  protected def compareSyncInfo: Receive = {
    case OtherNodeSyncingInfo(remote, syncInfo: SI) =>
      log.debug(s"Comparing remote info having starting points: ${syncInfo.startingPoints.map(_._2).toList
        .map(Base58.encode)}")
      syncInfo.startingPoints.map(_._2).headOption.foreach { head =>
        log.debug(s"Local side contains head: ${Base58.encode(head)}")
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

  protected def getSyncInfo: Receive = {
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
    val SyntacticallyFailedPersistentModifier = Value(2)
    val SemanticallyFailedPersistentModifier = Value(3)
    val SuccessfulTransaction = Value(4)
    val SuccessfulSyntacticallyValidModifier = Value(5)
    val SuccessfulSemanticallyValidModifier = Value(6)


    //starting persistent modifier application. The application could be slow
    val StartingPersistentModifierApplication = Value(7)

    val OpenSurfaceChanged = Value(8)
    val StateChanged = Value(9)

    //rollback failed, really wrong situation, probably
    val FailedRollback = Value(10)

    val DownloadNeeded = Value(11)
  }

  //a command to subscribe for events
  case class Subscribe(events: Seq[EventType.Value])

  trait NodeViewHolderEvent

  case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                    status: History.HistoryComparisonResult.Value,
                                                    remoteSyncInfo: SI,
                                                    localSyncInfo: SI,
                                                    extension: Option[Seq[(ModifierTypeId, ModifierId)]])

  //node view holder starting persistent modifier application
  case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

  //hierarchy of events regarding modifiers application outcome
  trait ModificationOutcome extends NodeViewHolderEvent

  case class FailedTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX, error: Throwable) extends ModificationOutcome

  case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX) extends ModificationOutcome

  case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

  case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

  case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

  //todo: separate classes instead of boolean flag?
  case class ChangedState(isRollback: Boolean, newVersion: VersionTag) extends NodeViewHolderEvent

  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  //todo: consider sending info on the rollback
  case object RollbackFailed extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}