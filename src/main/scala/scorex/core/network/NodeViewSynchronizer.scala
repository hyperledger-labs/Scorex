package scorex.core.network


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.consensus.{History, HistoryReader, SyncInfo}
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.network.peer.PeerManager
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{PersistentNodeViewModifier, _}
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

/**
  * A component which is synchronizing local node view (locked inside NodeViewHolder) with the p2p network.
  *
  * @param networkControllerRef reference to network controller actor
  * @param viewHolderRef        reference to node view holder actor
  * @param localInterfaceRef    reference to local interface actor
  * @param syncInfoSpec         SyncInfo specification
  * @tparam P   proposition
  * @tparam TX  transaction
  * @tparam SIS SyncInfoMessage specification
  */
class NodeViewSynchronizer[P <: Proposition,
TX <: Transaction[P],
SI <: SyncInfo,
SIS <: SyncInfoMessageSpec[SI],
PMOD <: PersistentNodeViewModifier,
HR <: HistoryReader[PMOD, SI],
MR <: MempoolReader[TX]](networkControllerRef: ActorRef,
                         viewHolderRef: ActorRef,
                         localInterfaceRef: ActorRef,
                         syncInfoSpec: SIS,
                         networkSettings: NetworkSettings,
                         timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import History._

  import NodeViewSynchronizer.ReceivableMessages._
  import scorex.core.NodeViewHolder.ReceivableMessages.{Subscribe, GetNodeViewChanges, CompareViews, ModifiersFromRemote}
  import scorex.core.network.NetworkController.ReceivableMessages.{SendToNetwork, RegisterMessagesHandler, SubscribePeerManagerEvent}
  import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  protected val deliveryTracker = new DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self)
  protected val statusTracker = new SyncTracker(self, context, networkSettings, localInterfaceRef, timeProvider)

  protected var historyReaderOpt: Option[HR] = None
  protected var mempoolReaderOpt: Option[MR] = None

  private def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  protected def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) =>
      broadcastModifierInv(tx)

    case FailedTransaction(tx, throwable) =>
    //todo: penalize source peer?

    case SyntacticallySuccessfulModifier(mod) =>
    case SyntacticallyFailedModification(mod, throwable) =>
    //todo: penalize source peer?

    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)

    case SemanticallyFailedModification(mod, throwable) =>
    //todo: penalize source peer?

    case ChangedHistory(reader: HR@unchecked) if reader.isInstanceOf[HR] =>
      //TODO isInstanceOf, type erasure
      historyReaderOpt = Some(reader)

    case ChangedMempool(reader: MR@unchecked) if reader.isInstanceOf[MR] =>
      //TODO isInstanceOf, type erasure
      mempoolReaderOpt = Some(reader)
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      statusTracker.updateStatus(remote, Unknown)

    case DisconnectedPeer(remote) =>
      statusTracker.clearStatus(remote)
  }

  protected def getLocalSyncInfo: Receive = {
    case SendLocalSyncInfo =>
      //todo: why this condition?
      if (statusTracker.elapsedTimeSinceLastSync() < (networkSettings.syncInterval.toMillis / 2)) {
        //TODO should never reach this point
        log.debug("Trying to send sync info too often")
      } else {
        historyReaderOpt.foreach(r => sendSync(r.syncInfo))
      }
  }

  protected def sendSync(syncInfo: SI): Unit = {
    val peers = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
  }

  //sync info is coming from another node
  protected def processSync: Receive = {
    case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>

      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt = historyReader.continuationIds(syncInfo, networkSettings.networkChunkSize)
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

  //object ids coming from other node
  protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.MessageCode =>

      //TODO can't replace viewHolderRef with a reader because of modifiers cache
      viewHolderRef ! CompareViews(remote, invData._1, invData._2)
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
    * Logic to process modifiers got from another peer
    */
  protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>

      val typeId = data._1
      val modifiers = data._2

      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${data._2.keySet.map(Base58.encode).mkString(",")}")

      for ((id, _) <- modifiers) deliveryTracker.receive(typeId, id, remote)

      val (spam, fm) = modifiers partition { case (id, _) => deliveryTracker.isSpam(id) }

      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Base58.encode)}")
        penalizeSpammingPeer(remote)
        val mids = spam.keys.toSeq
        deliveryTracker.deleteSpam(mids)
      }

      if (fm.nonEmpty) {
        val mods = fm.values.toSeq
        viewHolderRef ! ModifiersFromRemote(remote, typeId, mods)
      }
  }

  //local node sending object ids to remote
  protected def requestFromLocal: Receive = {
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>

      if (modifierIds.nonEmpty) {
        val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
        peer.handlerRef ! msg
      }
      deliveryTracker.expect(peer, modifierTypeId, modifierIds)
  }

  // todo: make DeliveryTracker an independent actor and move checkDelivery there?

  //scheduler asking node view synchronizer to check whether requested messages have been delivered
  protected def checkDelivery: Receive = {
    case CheckDelivery(peer, modifierTypeId, modifierId) =>

      if (deliveryTracker.peerWhoDelivered(modifierId).contains(peer)) {
        deliveryTracker.delete(modifierId)
      }
      else {
        log.info(s"Peer $peer has not delivered asked modifier ${Base58.encode(modifierId)} on time")
        penalizeNonDeliveringPeer(peer)
        deliveryTracker.reexpect(peer, modifierTypeId, modifierId)
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

  //local node sending out objects requested to remote
  protected def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val modType = modifiers.head.modifierTypeId
        val m = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg = Message(ModifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
  }

  override def preStart(): Unit = {
    //register as a handler for synchronization-specific types of messages
    val messageSpecs = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessagesHandler(messageSpecs, self)

    //register as a listener for peers got connected (handshaked) or disconnected
    val pmEvents = Seq(
      PeerManager.HandshakedEvent,
      PeerManager.DisconnectedEvent
    )
    networkControllerRef ! SubscribePeerManagerEvent(pmEvents)


    //subscribe for all the node view holder events involving modifiers and transactions
    val vhEvents = Seq(
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.MempoolChanged,
      NodeViewHolder.EventType.FailedTransaction,
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SemanticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
      NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier
    )
    viewHolderRef ! Subscribe(vhEvents)
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    statusTracker.scheduleSendSyncInfo()
  }

  override def receive: Receive =
    getLocalSyncInfo orElse
      processSync orElse
      processSyncStatus orElse
      processInv orElse
      modifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse
      modifiersFromRemote orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewSynchronizer {
  object ReceivableMessages {
    // getLocalSyncInfo messages
    case object SendLocalSyncInfo
    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])
    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])
    case class CheckDelivery(source: ConnectedPeer,
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
    //TODO: return mempool reader
    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction[_]]](mempool: MR) extends NodeViewChange
    //TODO: return Vault reader
    //FIXME: No actor process this message explicitly, it seems to be sent to NodeViewSynchcronizer
    case class ChangedVault() extends NodeViewChange

    //hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent
    case class FailedTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX, error: Throwable) extends ModificationOutcome
    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX) extends ModificationOutcome
    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome
    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome
    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome
    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome
  }
}

object NodeViewSynchronizerRef {
  def props[P <: Proposition,
    TX <: Transaction[P],
    SI <: SyncInfo,
    SIS <: SyncInfoMessageSpec[SI],
    PMOD <: PersistentNodeViewModifier,
    HR <: HistoryReader[PMOD, SI],
    MR <: MempoolReader[TX]](networkControllerRef: ActorRef,
                             viewHolderRef: ActorRef,
                             localInterfaceRef: ActorRef,
                             syncInfoSpec: SIS,
                             networkSettings: NetworkSettings,
                             timeProvider: NetworkTimeProvider) =
    Props(new NodeViewSynchronizer[P, TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef,
                                                                 localInterfaceRef, syncInfoSpec,
                                                                 networkSettings, timeProvider))

  def apply[P <: Proposition,
    TX <: Transaction[P],
    SI <: SyncInfo,
    SIS <: SyncInfoMessageSpec[SI],
    PMOD <: PersistentNodeViewModifier,
    HR <: HistoryReader[PMOD, SI],
    MR <: MempoolReader[TX]](networkControllerRef: ActorRef,
                             viewHolderRef: ActorRef,
                             localInterfaceRef: ActorRef,
                             syncInfoSpec: SIS,
                             networkSettings: NetworkSettings,
                             timeProvider: NetworkTimeProvider)
                            (implicit system: ActorSystem): ActorRef =
    system.actorOf(props[P, TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef, localInterfaceRef,
                                                       syncInfoSpec, networkSettings, timeProvider))

  def apply[P <: Proposition,
    TX <: Transaction[P],
    SI <: SyncInfo,
    SIS <: SyncInfoMessageSpec[SI],
    PMOD <: PersistentNodeViewModifier,
    HR <: HistoryReader[PMOD, SI],
    MR <: MempoolReader[TX]](name: String,
                             networkControllerRef: ActorRef,
                             viewHolderRef: ActorRef,
                             localInterfaceRef: ActorRef,
                             syncInfoSpec: SIS,
                             networkSettings: NetworkSettings,
                             timeProvider: NetworkTimeProvider)
                            (implicit system: ActorSystem): ActorRef =
    system.actorOf(props[P, TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef, localInterfaceRef,
                                                       syncInfoSpec, networkSettings, timeProvider), name)
}