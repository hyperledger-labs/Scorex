package scorex.core.network


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.NodeViewHolder.DownloadRequest
import scorex.core.NodeViewHolder.ReceivableMessages.{ChangedCache, GetNodeViewChanges, TransactionsFromRemote}
import scorex.core.consensus.History._
import scorex.core.consensus.{History, HistoryReader, SyncInfo}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.serialization.Serializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.state.StateReader
import scorex.core.transaction.wallet.VaultReader
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding, ScorexLogging}
import scorex.core.validation.MalformedModifierError
import scorex.core.{PersistentNodeViewModifier, _}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/**
  * A component which is synchronizing local node view (locked inside NodeViewHolder) with the p2p network.
  *
  * @param networkControllerRef reference to network controller actor
  * @param viewHolderRef        reference to node view holder actor
  * @param syncInfoSpec         SyncInfo specification
  * @tparam TX  transaction
  * @tparam SIS SyncInfoMessage specification
  */
class NodeViewSynchronizer[TX <: Transaction,
SI <: SyncInfo,
SIS <: SyncInfoMessageSpec[SI],
PMOD <: PersistentNodeViewModifier,
HR <: HistoryReader[PMOD, SI] : ClassTag,
MR <: MempoolReader[TX] : ClassTag]
(networkControllerRef: ActorRef,
 viewHolderRef: ActorRef,
 syncInfoSpec: SIS,
 networkSettings: NetworkSettings,
 timeProvider: NetworkTimeProvider,
 modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]])(implicit ec: ExecutionContext) extends Actor
  with ScorexLogging with ScorexEncoding {

  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  protected lazy val modifiersCache: ModifiersCache[PMOD, HR] =
    new DefaultModifiersCache[PMOD, HR](networkSettings.maxModifiersCacheSize)

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  protected val modifiersSpec = new ModifiersSpec(networkSettings.maxPacketSize)

  protected val deliveryTracker = new DeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks, self)
  protected val statusTracker = new SyncTracker(self, context, networkSettings, timeProvider)

  protected var historyReaderOpt: Option[HR] = None
  protected var mempoolReaderOpt: Option[MR] = None

  override def preStart(): Unit = {
    //register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, modifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessagesHandler(messageSpecs, self)

    //register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    //subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[HR]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[MR]])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.eventStream.subscribe(self, classOf[ModifiersAppliedFromCache[PMOD]])
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    statusTracker.scheduleSendSyncInfo()
  }

  private def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  protected def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) =>
      deliveryTracker.onApply(tx.id)
      broadcastModifierInv(tx)

    case FailedTransaction(tx, _) =>
      deliveryTracker.onInvalid(tx.id)
    //todo: penalize source peer?

    case SyntacticallySuccessfulModifier(mod) =>
      deliveryTracker.onApply(mod.id)

    case SyntacticallyFailedModification(mod, _) =>
      deliveryTracker.onInvalid(mod.id)
    //todo: penalize source peer?

    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)

    case SemanticallyFailedModification(_, _) =>
    //todo: penalize source peer?

    case ChangedHistory(reader: HR) =>
      historyReaderOpt = Some(reader)

    case ChangedMempool(reader: MR) =>
      mempoolReaderOpt = Some(reader)

    case ModifiersAppliedFromCache(applied: Seq[PMOD]) =>
      requestMoreModifiers(applied)
  }

  /**
    * Application-specific logic to request more modifiers after application if needed to
    * speed-up synchronization process, e.g. send Sync message for unknown or older peers
    * when our modifier is not synced yet, but no modifiers are expected from other peers
    * or request modifiers we need with known ids, that are not applied yet.
    */
  protected def requestMoreModifiers(applied: Seq[PMOD]): Unit = {}

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      statusTracker.updateStatus(remote, Unknown)

    case DisconnectedPeer(remote) =>
      statusTracker.clearStatus(remote)
  }

  protected def getLocalSyncInfo: Receive = {
    case SendLocalSyncInfo =>
      historyReaderOpt.foreach(r => sendSync(statusTracker, r))
  }

  protected def sendSync(syncTracker: SyncTracker, history: HR): Unit = {
    val peers = statusTracker.peersToSyncWith()
    if (peers.nonEmpty) {
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(history.syncInfo), None), SendToPeers(peers))
    }
  }

  //sync info is coming from another node
  protected def processSync: Receive = {
    case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>

      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt = historyReader.continuationIds(syncInfo, networkSettings.desiredInvObjects)
          val ext = extensionOpt.getOrElse(Seq())
          val comparison = historyReader.compare(syncInfo)
          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.trace(s"Extension ids: ${idsToString(ext)}")

          if (!(extensionOpt.nonEmpty || comparison != Younger)) {
            log.warn("Extension is empty while comparison is younger")
          }

          self ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
  }


  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.warn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) =>
      ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
        case (mid, mods) =>
          networkControllerRef ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
      }
  }

  //view holder is telling other node status
  protected def processSyncStatus: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)

      status match {
        case Unknown =>
          //todo: should we ban peer if its status is unknown after getting info from it?
          log.warn("Peer status is still unknown")
        case Nonsense =>
          //todo: fix, see https://github.com/ScorexFoundation/Scorex/issues/158
          log.warn("Got nonsense")
        case Younger =>
          sendExtension(remote, status, extOpt)
        case _ => // does nothing for `Equal` and `Older`
      }
  }

  /**
    * Object ids coming from other node.
    * Filter out modifier ids that are already in process (requested, received or applied),
    * request unknown ids from peer and set this ids to requested state.
    */
  protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, peer)
      if spec.messageCode == InvSpec.MessageCode =>

      (mempoolReaderOpt, historyReaderOpt) match {
        case (Some(mempool), Some(history)) =>
          val modifierTypeId = invData._1
          val newModifierIds = modifierTypeId match {
            case Transaction.ModifierTypeId =>
              invData._2.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            case _ =>
              invData._2.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
          }

          if (newModifierIds.nonEmpty) {
            val msg = Message(requestModifierSpec, Right(modifierTypeId -> newModifierIds), None)
            peer.handlerRef ! msg
            deliveryTracker.onRequest(Some(peer), modifierTypeId, newModifierIds)
          }

        case _ =>
          log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
      }
  }

  //other node asking for objects by their ids
  protected def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>

      readersOpt.foreach { readers =>
        val objs: Seq[NodeViewModifier] = invData._1 match {
          case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
            readers._2.getAll(invData._2)
          case _: ModifierTypeId =>
            invData._2.flatMap(id => readers._1.modifierById(id))
        }

        log.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
        self ! ResponseFromLocal(remote, invData._1, objs)
      }
  }

  /**
    * Logic to process modifiers got from another peer.
    * Filter out non-requested modifiers (with a penalty to spamming peer),
    * parse modifiers and send valid modifiers to NodeViewHolder
    */
  protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.MessageCode =>

      val typeId = data._1
      val modifiers = data._2
      log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${modifiers.keySet.map(encoder.encode).mkString(",")}")

      // filter out non-requested modifiers
      val requestedModifiers = processSpam(remote, typeId, modifiers)

      modifierSerializers.get(typeId) match {
        case Some(serializer: Serializer[TX]@unchecked) if typeId == Transaction.ModifierTypeId =>
          // parse all transactions and send them to node view holder
          val parsed: Iterable[TX] = parseModifiers(requestedModifiers, serializer, remote)
          viewHolderRef ! TransactionsFromRemote(parsed)

        case Some(serializer: Serializer[PMOD]@unchecked) =>
          // parse all modifiers and put them to modifiers cache
          val parsed: Iterable[PMOD] = parseModifiers(requestedModifiers, serializer, remote)
          if (parsed.nonEmpty) {
            parsed.foreach(pmod => putToCacheIfValid(remote, pmod))

            // remove elements from cache if it's size exceeds the limit, reset status for removed modifiers
            val cleared = modifiersCache.cleanOverfull()
            if (cleared.nonEmpty) {
              log.debug(s"${cleared.size} modifiers ${idsToString(typeId, cleared.map(_.id))} were removed from cache")
              cleared.foreach(removed => deliveryTracker.stopProcessing(removed.id))
            }
            // send changed cache to node view holder to try to apply new modifiers
            viewHolderRef ! ChangedCache[PMOD, HR, ModifiersCache[PMOD, HR]](modifiersCache)
          }
        case _ =>
          log.error(s"Undefined serializer for modifier of type $typeId")
      }
  }

  /**
    * Put `pmod` to `modifiersCache` if `pmod` is valid or may be valid in the future,
    * penalize peer if `pmod` is permanently invalid
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  private def putToCacheIfValid(remote: ConnectedPeer, pmod: PMOD): Unit = {
    historyReaderOpt match {
      case Some(hr) =>
        hr.applicableTry(pmod) match {
          case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
            log.warn(s"Modifier ${pmod.encodedId} is permanently invalid", e)
            deliveryTracker.onInvalid(pmod.id)
            penalizeMisbehavingPeer(remote)
          case _ =>
            modifiersCache.put(pmod.id, pmod)
        }
      case None =>
        log.error("Got modifier while history reader is not ready")
        modifiersCache.put(pmod.id, pmod)
    }
  }

  /**
    * Parse modifiers using specified serializer, check that it's id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier,
    * call deliveryTracker.onReceive() for every correct modifier to update it's status
    *
    * @return collection of parsed modifiers
    */
  private def parseModifiers[M <: NodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                                    serializer: Serializer[M],
                                                    remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      serializer.parseBytes(bytes) match {
        case Success(mod) if id == mod.id =>
          deliveryTracker.onReceive(id)
          Some(mod)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse modifier with declared id ${encoder.encode(id)} from ${remote.toString}")
          None
      }
    }
  }

  /**
    * Get modifiers from remote peer,
    * filter out spam modifiers and penalize peer for spam
    *
    * @return ids and bytes of modifiers that were requested by our node
    */
  private def processSpam(remote: ConnectedPeer,
                          typeId: ModifierTypeId,
                          modifiers: Map[ModifierId, Array[Byte]]): Map[ModifierId, Array[Byte]] = {

    val (requested, spam) = modifiers.partition { case (id, _) =>
      deliveryTracker.status(id) == Requested
    }

    if (spam.nonEmpty) {
      log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
        s": ${spam.keys.map(encoder.encode)}")
      penalizeSpammingPeer(remote)
    }
    requested
  }

  /**
    * Scheduler asking node view synchronizer to check whether requested modifiers have been delivered.
    * Do nothing, if modifier is already in a different state (it might be already received, applied, etc.),
    * wait for delivery until the number of checks does not exceed the maximum if the peer sent `Inv` for this modifier
    * re-request modifier from a different random peer, if our node do not know a peer who have it
    */
  protected def checkDelivery: Receive = {
    case CheckDelivery(peerOpt, modifierTypeId, modifierId) =>
      if (deliveryTracker.status(modifierId) == ModifiersStatus.Requested) {
        peerOpt match {
          case Some(peer) =>
            log.info(s"Peer ${peer.toString} has not delivered asked modifier ${encoder.encode(modifierId)} on time")
            penalizeNonDeliveringPeer(peer)
            deliveryTracker.onStillWaiting(peer, modifierTypeId, modifierId)
          case None =>
            // Random peer did not delivered modifier we need, ask another peer
            // We need this modifier - no limit for number of attempts
            log.info(s"Modifier ${encoder.encode(modifierId)} was not delivered on time")
            deliveryTracker.stopProcessing(modifierId)
            requestDownload(modifierTypeId, Seq(modifierId))
        }
      }
  }

  protected def penalizeNonDeliveringPeer(peer: ConnectedPeer): Unit = {
    //todo: do something less harsh than blacklisting?
    //todo: proposal: add a new field to PeerInfo to count how many times
    //todo: the peer has been penalized for not delivering. In PeerManager,
    //todo: add something similar to FilterPeers to return only peers that
    //todo: have not been penalized too many times.

    // networkControllerRef ! Blacklist(peer)
  }

  protected def penalizeSpammingPeer(peer: ConnectedPeer): Unit = {
    //todo: consider something less harsh than blacklisting, see comment for previous function
    // networkControllerRef ! Blacklist(peer)
  }

  protected def penalizeMisbehavingPeer(peer: ConnectedPeer): Unit = {
    // todo: peer sent incorrect modifier - blacklist or another serious penalty required
  }

  /**
    * Local node sending out objects requested to remote
    */
  protected def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      modifiers.headOption.foreach { head =>
        val modType = head.modifierTypeId
        val m = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg = Message(modifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
  }

  /**
    * Our node needs modifiers of type `modifierTypeId` with ids `modifierIds`
    * but peer that can deliver it is unknown.
    * Request this modifier from random peer.
    */
  protected def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    deliveryTracker.onRequest(None, modifierTypeId, modifierIds)
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    //todo: A peer which is supposedly having the modifier should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      if (deliveryTracker.status(modifierId, historyReaderOpt.toSeq) == ModifiersStatus.Unknown) {
        requestDownload(modifierTypeId, Seq(modifierId))
      }
  }

  override def receive: Receive =
    onDownloadRequest orElse
      getLocalSyncInfo orElse
      processSync orElse
      processSyncStatus orElse
      processInv orElse
      modifiersReq orElse
      responseFromLocal orElse
      modifiersFromRemote orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }

}

object NodeViewSynchronizer {

  object Events {

    trait NodeViewSynchronizerEvent

    case object NoBetterNeighbour extends NodeViewSynchronizerEvent

    case object BetterNeighbourAppeared extends NodeViewSynchronizerEvent

  }

  object ReceivableMessages {

    // getLocalSyncInfo messages
    case object SendLocalSyncInfo

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    /**
      * Check delivery of modifier with type `modifierTypeId` and id `modifierId`.
      * `source` may be defined if we expect modifier from concrete peer or None if
      * we just need some modifier, but don't know who have it
      *
      */
    case class CheckDelivery(source: Option[ConnectedPeer],
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction]](mempool: MR) extends NodeViewChange

    case class ChangedVault[VR <: VaultReader](reader: VR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    //todo: consider sending info on the rollback

    case object RollbackFailed extends NodeViewHolderEvent

    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

    /**
      * After application of batch of modifiers from cache to History,
      * NodeViewHolder sends this message, containing all just applied modifiers
      */
    case class ModifiersAppliedFromCache[PMOD <: PersistentNodeViewModifier](modifiers: Seq[PMOD])

    //hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    case class FailedTransaction[TX <: Transaction](transaction: TX, error: Throwable) extends ModificationOutcome

    case class SuccessfulTransaction[TX <: Transaction](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}

object NodeViewSynchronizerRef {
  def props[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MempoolReader[TX] : ClassTag]
  (networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]])(implicit ec: ExecutionContext): Props =
    Props(new NodeViewSynchronizer[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef, syncInfoSpec,
      networkSettings, timeProvider, modifierSerializers))

  def apply[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MempoolReader[TX] : ClassTag]
  (networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]])
  (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider, modifierSerializers))

  def apply[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MempoolReader[TX] : ClassTag]
  (name: String,
   networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]])
  (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider, modifierSerializers), name)
}
