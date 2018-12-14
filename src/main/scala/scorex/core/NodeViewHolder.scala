package scorex.core

import akka.actor.{Actor, ActorRef}
import akka.pattern._
import akka.util.Timeout
import scorex.core.NodeViewComponent._
import scorex.core.NodeViewComponentOperation.GetReader
import scorex.core.consensus.{HistoryReader, SyncInfo}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.settings.ScorexSettings
import scorex.core.transaction._
import scorex.core.transaction.state.{MinimalState, StateHistoryActor, StateReader}
import scorex.core.transaction.wallet.VaultReader
import scorex.util.{ScorexEncoding, ScorexLogging}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}


/** Composite local view of the node.
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * The main data structure a node software is taking care about, a node view consists
  * of four elements to be updated atomically: history (log of persistent modifiers),
  * state (result of log's modifiers application to pre-historical(genesis) state,
  * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
  */
trait NodeViewHolder[TX <: Transaction, PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging with ScorexEncoding {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
  import scorex.core.transaction.MempoolOperation._
  import scorex.core.transaction.state.StateOperation._

  type SI <: SyncInfo
  type History <: consensus.History[PMOD, SI, History]
  type State <: MinimalState[PMOD, State]
  type Vault <: transaction.wallet.Vault[TX, PMOD, Vault]
  type MPool <: MempoolReader[TX]

  /** State actor used for asynchronous transaction validation
    */
  protected val stateActor: ActorRef = createStateActor()

  /** Underlying memory pool actor
    */
  protected val memoryPoolActor: ActorRef = createMemoryPoolActor()

  protected var vault: Vault = restoreVault().getOrElse(genesisVault())

  val scorexSettings: ScorexSettings

  protected implicit val defaultAskTimeout: Timeout = Timeout(10.seconds)
  protected implicit val executionContext: ExecutionContext = context.dispatcher

  /** Restore a local history during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  protected def restoreHistory(): Option[History]

  /** Restore a local state during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  protected def restoreState(): Option[State]

  /** Restore a local vault during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  protected def restoreVault(): Option[Vault]

  /** Restore a local memory pool during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  protected def restoreMempool(): Option[MPool]

  /** Hard-coded initial history all the honest nodes in a network are making progress from
    */
  protected def genesisHistory(): History

  /** Hard-coded initial state all the honest nodes in a network are making progress from
    */
  protected def genesisState(): State

  /** Hard-coded initial state all the honest nodes in a network are making progress from
    */
  protected def genesisVault(): Vault

  /** Hard-coded initial history all the honest nodes in a network are making progress from
    */
  protected def genesisMempool(): MPool

  protected def createMemoryPoolActor(): ActorRef

  protected def createStateActor(): ActorRef = {
    StateHistoryActor[TX, PMOD, SI, State, History](restoreState().getOrElse(genesisState()),
      restoreHistory().getOrElse(genesisHistory()))(context.system)
  }

  /** Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  protected lazy val modifiersCache: ModifiersCache[PMOD, HistoryReader[PMOD, SI]] =
    new DefaultModifiersCache[PMOD, HistoryReader[PMOD, SI]](scorexSettings.network.maxModifiersCacheSize)

  protected def extractTransactions(mod: PMOD): Seq[TX] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[TX] => tcm.transactions
    case _ => Seq()
  }

  protected def processModifiers: Receive = {
    case msg: ModifiersFromRemote[PMOD] => processRemoteModifiers(msg.modifiers)
    case msg: LocallyGeneratedModifier[PMOD] => processLocalModifier(msg.pmod)
    case response: PersistentModifierResponse[PMOD] => persistentModifierResponse(response)
    case CleanCacheOverfull => cleanCacheOvefull()
  }

  protected def processLocalModifier(modifier: PMOD): Unit = {
    log.info(s"Got locally generated modifier ${modifier.encodedId} of type ${modifier.modifierTypeId}")
    stateActor ! ApplyModifier(modifier, LocallyGenerated)
  }

  /** Process new modifiers from remote.
    * Put all candidates to modifiersCache and then try to apply as much modifiers from cache as possible.
    * Clear cache if it's size exceeds size limit.
    * Publish `ModifiersProcessingResult` message with all just applied and removed from cache modifiers.
    */
  protected def processRemoteModifiers(mods: Iterable[PMOD]): Unit = {
    mods.foreach(m => modifiersCache.put(m.id, m))
    log.debug(s"Cache size before: ${modifiersCache.size}")
    nextRemoteModifier()
  }

  protected def nextRemoteModifier(): Unit = {
    requestReader[HistoryReader[PMOD, SI]](HistoryComponent) { history =>
      modifiersCache.popCandidate(history) match {
        case Some(mod) => stateActor ! ApplyModifier(mod, RemotelyGenerated)
        case None => self ! CleanCacheOverfull
      }
    }
  }

  protected def cleanCacheOvefull(): Unit ={
    val cleared = modifiersCache.cleanOverfull()
    log.debug(s"Cache size after application: ${modifiersCache.size}")
    context.system.eventStream.publish(ModifiersProcessingResult(Seq.empty, cleared))
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  protected def persistentModifierResponse(response: PersistentModifierResponse[PMOD]): Unit = {
    val PersistentModifierResponse(updatedComponents, progressInfo, blocksApplied, pmod, mode) = response
    publishNodeView(updatedComponents)
    if (updatedComponents.contains(HistoryComponent)) {
      progressInfo.toDownload.foreach { case (tid, id) =>
        context.system.eventStream.publish(DownloadRequest(tid, id))
      }
    }
    if (updatedComponents.contains(StateComponent) &&  progressInfo.toApply.nonEmpty) {
      updateMemPool(progressInfo.toRemove, blocksApplied)
      if (progressInfo.chainSwitchingNeeded) {
        //we consider that vault always able to perform a rollback needed
        vault = vault.rollback(idToVersion(progressInfo.branchPoint.get)).get
      }
      blocksApplied.foreach(vault.scanPersistent)
      context.system.eventStream.publish(ChangedVault(vault.getReader))
      publishNodeView(Set(VaultComponent, MempoolComponent))
    }
    if (mode == RemotelyGenerated) {
      context.system.eventStream.publish(ModifiersProcessingResult(Seq(pmod), Seq.empty))
      nextRemoteModifier()
    }
  }

  protected def updateMemPool(blocksRemoved: Seq[PMOD], blocksApplied: Seq[PMOD]): Unit = {
    val rolledBackTxs = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs = blocksApplied.flatMap(extractTransactions)
    val appliedIds = appliedTxs.map(_.id).toSet
    val txsToPut = rolledBackTxs.filterNot(tx => appliedIds.contains(tx.id))
    txsToPut.foreach(tx => stateActor ! ValidateTransaction(tx, PutWithoutCheck))
  }

  protected def processNewTransactions: Receive = {
    case newTxs: NewTransactions[TX] => txModify(newTxs.txs)
    case validationResponse: TransactionValidationResponse[TX] => txValidationResponse(validationResponse)
    case putResponse: PutResponse[TX] => txPutResponse(putResponse)
  }

  protected def txModify(txs: Iterable[TX]): Unit = {
    txs.foreach(tx => stateActor ! ValidateTransaction(tx))
  }

  protected def txValidationResponse(response: TransactionValidationResponse[TX]): Unit = {
    response match {
      case TransactionValidationResponse(tx, Success(()), mode) =>
        memoryPoolActor ! Put(tx, mode)

      case TransactionValidationResponse(tx, Failure(e), _) =>
        context.system.eventStream.publish(FailedTransaction[TX](tx, e))
    }
  }

  protected def txPutResponse(response: PutResponse[TX]): Unit = response match {
    case PutResponse(_, _, PutWithoutCheck) =>
    //ignore

    case PutResponse(tx, Failure(e), _) =>
      context.system.eventStream.publish(FailedTransaction[TX](tx, e))

    case PutResponse(tx, Success(()), _) =>
      log.debug(s"Unconfirmed transactions $tx added to the memory pool")
      context.system.eventStream.publish(SuccessfulTransaction(tx))
      vault = vault.scanOffchain(tx)
      context.system.eventStream.publish(ChangedVault(vault.getReader))
      publishNodeView(Set(MempoolComponent, VaultComponent))
  }

  /** Request current node view and perform small transformation with it;
    * `callback` should be a lightweight lambda operation to transform result
    * and it should not access any actor's state.
    */
  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      val callback = sender
      for {
        historyReader <- getReader(HistoryComponent)
        stateReader <- getReader(StateComponent)
        mempoolReader <- getReader(MempoolComponent)
      } yield callback ! f(CurrentView(historyReader, stateReader, vault, mempoolReader))
  }

  protected def getNodeViewChanges: Receive = {
    case GetNodeViewChanges(components) =>
      val callback = sender
      components.foreach { c => requestChangeEvent(c)(e => callback ! e) }
  }

  protected def publishNodeView(components: Set[ComponentType]): Unit = {
    components.foreach { c => requestChangeEvent(c)(e => context.system.eventStream.publish(e)) }
  }

  /** Generate change event and perform small operation with it;
    * `callback` should not be a heavy operation and it should not access any actor's state.
    * @param componentType type of the component to get the reader
    * @param callback lightweight lambda, it shouldn't access any actor's state
    */
  protected def requestChangeEvent(component: ComponentType)(callback: NodeViewChange => Unit): Unit = component match {
    case MempoolComponent => requestReader[MempoolReader[TX]](component){ r => callback(ChangedMempool(r)) }
    case StateComponent => requestReader[StateReader](component){ r => callback(ChangedState(r)) }
    case HistoryComponent => requestReader[HistoryReader[PMOD, SI]](component){ r => callback(ChangedHistory(r)) }
    case VaultComponent =>  requestReader[VaultReader](component){ r => callback(ChangedVault(r)) }
  }

  /** Request reader and perform small operation with it;
    * `callback` should not be a heavy operation and it should not access any actor's state.
    * @param componentType type of the component to get the reader
    * @param callback lightweight lambda, it shouldn't access any actor's state
    */
  protected def requestReader[R : ClassTag](componentType: ComponentType)(callback: R => Unit): Unit = {
    getReader(componentType).mapTo[R].onComplete {
      case Success(reader) => callback(reader)
      case Failure(e) => log.error(s"Failed to get $toString reader: ${e.getMessage}", e)
    }
  }

  def getReader(componentType: ComponentType): Future[Any] =  componentType match {
    case MempoolComponent => memoryPoolActor ? GetReader(componentType)
    case StateComponent => stateActor ? GetReader(componentType)
    case HistoryComponent => stateActor ? GetReader(componentType)
    case VaultComponent => Future.successful(vault.getReader)
  }

  override def receive: Receive =
    processModifiers orElse
      processNewTransactions orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewHolder {

  object ReceivableMessages {

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(components: Set[ComponentType])

    /**  Request current node view and perform small transformation with it;
      * `callback` should not be a heavy operation  and it should not access any actor's state.
      *
      * @param callback lightweight lambda to transform result, it shouldn't access any actor's state
      */
    case class GetDataFromCurrentView[HIS, State, Vault, MPool, A](callback: CurrentView[HIS, State, Vault, MPool] => A)

    // Modifiers received from the remote peer with new elements in it
    case class ModifiersFromRemote[PM <: PersistentNodeViewModifier](modifiers: Iterable[PM])

    sealed trait NewTransactions[TX <: Transaction] {
      val txs: Iterable[TX]
    }

    case class LocallyGeneratedTransaction[TX <: Transaction](tx: TX) extends NewTransactions[TX] {
      override val txs: Iterable[TX] = Iterable(tx)
    }

    case class TransactionsFromRemote[TX <: Transaction](txs: Iterable[TX]) extends NewTransactions[TX]

    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

    case object CleanCacheOverfull

  }

  // fixme: No actor is expecting this ModificationApplicationStarted and DownloadRequest messages
  // fixme: Even more, ModificationApplicationStarted seems not to be sent at all
  // fixme: should we delete these messages?
  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: scorex.util.ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}
