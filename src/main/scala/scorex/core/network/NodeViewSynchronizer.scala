package scorex.core.network


import akka.actor.{Actor, ActorRef, Cancellable}
import scorex.core.NodeViewHolder._
import scorex.core.consensus.{History, HistoryReader, SyncInfo}
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.network.peer.PeerManager
import scorex.core.network.peer.PeerManager.HandshakedPeer
import scorex.core.network.peer.PeerManager.DisconnectedPeer
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{PersistentNodeViewModifier, _}
import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
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
HR <: HistoryReader[PMOD, SI],
MR <: MempoolReader[TX]](networkControllerRef: ActorRef,
                         viewHolderRef: ActorRef,
                         localInterfaceRef: ActorRef,
                         syncInfoSpec: SIS,
                         networkSettings: NetworkSettings,
                         timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import NodeViewSynchronizer._
  import History.HistoryComparisonResult._

  type Timestamp = Long

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val deliveryTracker = new DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self)

  /**
    We cache peers along with their statuses (whether another peer is ahead or behind of ours,
    or comparison is not possible, or status is not yet known)
   **/
  protected val statuses = mutable.Map[ConnectedPeer, HistoryComparisonResult.Value]()
  protected val statusUpdated = mutable.Map[ConnectedPeer, Timestamp]()

  protected var stableSyncRegime = false
  protected var scheduler: Cancellable = _

  protected var lastSyncInfoSentTime: Long = 0L
  protected var historyReaderOpt: Option[HR] = None
  protected var mempoolReaderOpt: Option[MR] = None

  def readers: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  private def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult.Value): Unit = {
    statuses.update(peer, status)
  }

  protected def updateTime(peer: ConnectedPeer): Unit = {
    statusUpdated.update(peer, timeProvider.time())
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

    scheduler = context.system.scheduler.schedule(2.seconds, networkSettings.syncInterval)(self ! SendLocalSyncInfo)

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
    case ChangedHistory(reader: HR@unchecked) if reader.isInstanceOf[HR] =>
      //TODO isInstanceOf ?
      //TODO type erasure
      historyReaderOpt = Some(reader)

    case ChangedMempool(reader: MR@unchecked) if reader.isInstanceOf[MR] =>
      //TODO isInstanceOf ?
      //TODO type erasure
      mempoolReaderOpt = Some(reader)
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      updateStatus(remote, HistoryComparisonResult.Unknown)
      updateTime(remote)

    case DisconnectedPeer(remote) => // todo: does nothing for now
  }

  /**
    * To send out regular sync signal, we first send a request to node view holder to get current syncing information
    */
  protected def getLocalSyncInfo: Receive = {
    case SendLocalSyncInfo =>
      val currentTime = timeProvider.time()
      if (currentTime - lastSyncInfoSentTime < (networkSettings.syncInterval.toMillis / 2)) {
        //TODO should never reach this point
        log.debug("Trying to send sync info too often")
      } else {
        lastSyncInfoSentTime = currentTime

        historyReaderOpt.foreach { r =>
          syncSend(r.syncInfo)
        }
      }
  }

  /**
    * Logic to send a sync signal to other peers: we whether send the signal to all the peers we haven't got status
    * for more than "syncStatusRefresh" setting says, or the signal is to be sent to all the unknown nodes and a random
    * peer which is older
    */
  protected def syncSend(syncInfo: SI): Unit = {
      val timeout = if(stableSyncRegime) networkSettings.syncStatusRefreshStable else networkSettings.syncStatusRefresh
      val minInterval = if(stableSyncRegime) networkSettings.syncIntervalStable else networkSettings.syncInterval

      val outdated = statusUpdated
        .filter(t => (System.currentTimeMillis() - t._2).millis > timeout)
        .keys
        .toSeq

      val peersToSend = if (outdated.nonEmpty) {
        outdated
      } else {
        val unknowns = statuses.filter(_._2 == HistoryComparisonResult.Unknown).keys.toIndexedSeq
        val olders = statuses.filter(_._2 == HistoryComparisonResult.Older).keys.toIndexedSeq
        if (olders.nonEmpty) olders(scala.util.Random.nextInt(olders.size)) +: unknowns else unknowns
      }.filter(peer => (System.currentTimeMillis() - statusUpdated(peer)).millis >= minInterval)

      if(peersToSend.nonEmpty) {
        peersToSend.foreach(updateTime)
        networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peersToSend))
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
          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}: ${idsToString(ext)}")

          if (!(extensionOpt.nonEmpty || comparison != HistoryComparisonResult.Younger)) {
            log.warn("Extension is empty while comparison is younger")
          }

          self ! OtherNodeSyncingStatus(
            remote,
            comparison,
            syncInfo,
            historyReader.syncInfo,
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

      if (seniorsBefore > 0 && seniorsAfter == 0) {
        log.info("Syncing is done, switching to stable regime")
        stableSyncRegime = true
        scheduler.cancel()
        scheduler = context.system.scheduler.schedule(2 seconds, networkSettings.syncIntervalStable)(self ! SendLocalSyncInfo)
        localInterfaceRef ! LocalInterface.NoBetterNeighbour
      }
      if (seniorsBefore == 0 && seniorsAfter > 0) {
        localInterfaceRef ! LocalInterface.BetterNeighbourAppeared
      }
  }

  //object ids coming from other node
  protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.MessageCode =>
      //TODO can't replace here because of modifiers cache

      viewHolderRef ! CompareViews(remote, invData._1, invData._2)
  }

  //other node asking for objects by their ids
  protected def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>

      readers.foreach { reader =>
        val objs: Seq[NodeViewModifier] = invData._1 match {
          case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
            reader._2.getAll(invData._2)
          case _: ModifierTypeId =>
            invData._2.flatMap(id => reader._1.modifierById(id))
        }

        log.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
        self ! NodeViewSynchronizer.ResponseFromLocal(remote, invData._1, objs)
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

      log.info(s"Got modifiers of type $typeId with ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      log.info(s"From remote connected peer: $remote")

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
        val modType = modifiers.head.modifierTypeId
        val m = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg = Message(ModifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
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

  case object SendLocalSyncInfo

  case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

  case class ModifiersFromRemote(source: ConnectedPeer, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

  case class CheckDelivery(source: ConnectedPeer,
                           modifierTypeId: ModifierTypeId,
                           modifierId: ModifierId)

}