package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder._
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.consensus.{History, HistoryReader, SyncInfo}
import scorex.core.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.network.peer.PeerManager
import scorex.core.network.peer.PeerManager.{DisconnectedPeer, HandshakedPeer}
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.{NetworkTime, ScorexLogging}
import scorex.core.{PersistentNodeViewModifier, _}
import scorex.crypto.encode.Base58

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * A middle layer between a node view holder(NodeViewHolder) and the p2p network
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
HR <: HistoryReader[PMOD, SI]](networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               localInterfaceRef: ActorRef,
                               syncInfoSpec: SIS,
                               networkSettings: NetworkSettings) extends Actor with ScorexLogging {

  import History.HistoryComparisonResult._
  import NodeViewSynchronizer._

  type Timestamp = Long

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val deliveryTracker = new DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self)

  /*
    We cache peers along with their statuses (whether another peer is ahead or behind of ours,
    or comparison is not possible, or status is not yet known)
   */
  protected val statuses = mutable.Map[ConnectedPeer, HistoryComparisonResult.Value]()
  protected val statusUpdated = mutable.Map[ConnectedPeer, Timestamp]()
  private var lastSyncInfoSentTime = NetworkTime.time()
  protected var historyReaderOpt: Option[HR] = None

  private def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult.Value): Unit = {
    statusUpdated.update(peer, System.currentTimeMillis())
    statuses.update(peer, status)
  }

  private def updateTime(peer: ConnectedPeer): Unit = {
    statusUpdated.update(peer, System.currentTimeMillis())
  }


  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  override def preStart(): Unit = {
    //register as a handler for some types of messages
    val messageSpecs = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)

    val pmEvents = Seq(
      PeerManager.EventType.Handshaked,
      PeerManager.EventType.Disconnected
    )

    networkControllerRef ! NetworkController.SubscribePeerManagerEvent(pmEvents)

    val vhEvents = Seq(
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.FailedTransaction,
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SemanticallyFailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
      NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier
    )
    viewHolderRef ! Subscribe(vhEvents)

    context.system.scheduler.schedule(2.seconds, networkSettings.syncInterval)(self ! GetLocalSyncInfo)
  }

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  protected def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case FailedTransaction(tx, throwable) =>
    //todo: ban source peer?

    case SyntacticallySuccessfulModifier(mod) =>
    case SyntacticallyFailedModification(mod, throwable) =>
    //todo: ban source peer?

    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    //todo: ban source peer?
    case ChangedHistory(reader) if reader.isInstanceOf[HR] =>
      //TODO isInstanceOf ?
      historyReaderOpt = Some(reader.asInstanceOf[HR])
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) => updateStatus(remote, HistoryComparisonResult.Unknown)
    case DisconnectedPeer(remote) => // todo: does nothing for now
  }

  /**
    * To send out regular sync signal, we first send a request to node view holder to get current syncing information
    */
  protected def getLocalSyncInfo: Receive = {
    case GetLocalSyncInfo =>
      historyReaderOpt.foreach { r =>
        self ! CurrentSyncInfo(r.syncInfo(false))
      }
  }

  /**
    * Logic to send a sync signal to other peers: we whether send the signal to all the peers we haven't got status
    * for more than "syncStatusRefresh" setting says, or the signal is to be sent to all the unknown nodes and a random
    * peer which is older
    */
  protected def syncSend: Receive = {
    case CurrentSyncInfo(syncInfo: SI@unchecked) =>
      val currentTime = NetworkTime.time()
      if (currentTime - lastSyncInfoSentTime < networkSettings.syncInterval.toMillis) {
        log.debug("Trying to send sync info too often")
      } else {
        lastSyncInfoSentTime = currentTime
        val outdated = statusUpdated
          .filter(t => (System.currentTimeMillis() - t._2).millis > networkSettings.syncStatusRefresh)
          .keys
          .toSeq
        if (outdated.nonEmpty) {
          outdated.foreach(updateTime)
          networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(outdated))
        } else {
          val unknowns = statuses.filter(_._2 == HistoryComparisonResult.Unknown).keys.toIndexedSeq
          val olders = statuses.filter(_._2 == HistoryComparisonResult.Older).keys.toIndexedSeq
          val candidates = if (olders.nonEmpty) {
            olders(scala.util.Random.nextInt(olders.size)) +: unknowns
          } else unknowns

          candidates.foreach(updateTime)
          networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(candidates))
        }
      }
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
          log.debug(s"Comparison with $remote having starting points ${History.idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}: ${History.idsToString(ext)}")

          if (!(extensionOpt.nonEmpty || comparison != HistoryComparisonResult.Younger)) {
            log.warn("Extension is empty while comparison is younger")
          }

          sender() ! OtherNodeSyncingStatus(
            remote,
            comparison,
            syncInfo,
            historyReader.syncInfo(true),
            extensionOpt
          )
        case _ =>
      }
  }

  //view holder is telling other node status
  protected def processSyncStatus: Receive = {
    case OtherNodeSyncingStatus(remote, status, remoteSyncInfo, localSyncInfo: SI@unchecked, extOpt) =>
      val seniorsBefore = statuses.count(_._2 == Older)

      updateStatus(remote, status)

      status match {
        case Nonsense =>
          //todo: we should ban peer if its view is totally different from ours
          log.warn("Got nonsense")
        case Equal =>
        case Older =>
        case Younger =>
          if (extOpt.isEmpty) {
            log.warn("extOpt is empty for Younger brother")
          } else {
            val ext = extOpt.get
            ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
              case (mid, mods) =>
                networkControllerRef ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
            }
          }
        //todo: should we ban peer if its status is unknown after getting info from it?
        case Unknown => log.warn("Peer status is still unknown")
      }

      val seniorsAfter = statuses.count(_._2 == Older)

      if (seniorsBefore > 0 && seniorsAfter == 0) localInterfaceRef ! LocalInterface.NoBetterNeighbour
      if (seniorsBefore == 0 && seniorsAfter > 0) localInterfaceRef ! LocalInterface.BetterNeighbourAppeared
  }

  //object ids coming from other node
  protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.MessageCode =>

      viewHolderRef ! CompareViews(remote, invData._1, invData._2)
  }

  //other node asking for objects by their ids
  protected def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>

      viewHolderRef ! GetLocalObjects(remote, invData._1, invData._2)
  }

  //other node is sending objects
  protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>

      val typeId = data._1
      val modifiers = data._2


      log.info(s"Got modifiers of type $typeId with ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      log.info(s"From remote connected peer: $remote")

      for ((id, _) <- modifiers) deliveryTracker.receive(typeId, id, remote)

      val (spam, fm) = modifiers partition {
        _ match {
          case (id, _) => deliveryTracker.isSpam(id)
        }
      }

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
        val modType = modifiers.head.modifierTypeId
        val m = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg = Message(ModifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
  }

  override def receive: Receive =
    getLocalSyncInfo orElse
      syncSend orElse
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

  case object GetLocalSyncInfo

  case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class GetLocalObjects(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

  case class ModifiersFromRemote(source: ConnectedPeer, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

  case class CheckDelivery(source: ConnectedPeer,
                           modifierTypeId: ModifierTypeId,
                           modifierId: ModifierId)

}