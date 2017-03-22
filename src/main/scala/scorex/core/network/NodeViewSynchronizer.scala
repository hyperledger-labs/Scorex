package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.{LocalInterface, NodeViewHolder, NodeViewModifier}
import scorex.core.NodeViewHolder._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A middle layer between a node view holder(NodeViewHolder) and a network
  *
  * @param networkControllerRef
  * @param viewHolderRef
  * @param localInterfaceRef
  * @param syncInfoSpec
  * @tparam P
  * @tparam TX
  * @tparam SIS
  */
class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P], SI <: SyncInfo, SIS <: SyncInfoMessageSpec[SI]]
(networkControllerRef: ActorRef,
 viewHolderRef: ActorRef,
 localInterfaceRef: ActorRef,
 syncInfoSpec: SIS) extends Actor with ScorexLogging {

  import NodeViewSynchronizer._
  import scorex.core.NodeViewModifier._
  import History.HistoryComparisonResult._

  //modifier ids asked from other nodes are kept in order to check then
  //against objects sent
  private val asked = mutable.Map[ModifierTypeId, mutable.Set[ModifierId]]()

  private val seniors = mutable.Set[String]()
  private val juniors = mutable.Set[String]()
  private val equals = mutable.Set[String]()

  override def preStart(): Unit = {
    //register as a handler for some types of messages
    val messageSpecs = Seq(InvSpec, RequestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)

    //subscribe for failed transaction,
    val events = Seq(
      NodeViewHolder.EventType.FailedTransaction,
      NodeViewHolder.EventType.FailedPersistentModifier,
      NodeViewHolder.EventType.SuccessfulTransaction,
      NodeViewHolder.EventType.SuccessfulPersistentModifier
    )
    viewHolderRef ! Subscribe(events)

    context.system.scheduler.schedule(2.seconds, 15.seconds)(self ! GetLocalSyncInfo)
  }

  private def sendModifierIfLocal[M <: NodeViewModifier](m: M, source: Option[ConnectedPeer]): Unit =
    if (source.isEmpty) {
      val msg = Message(InvSpec, Right(Transaction.ModifierTypeId -> Seq(m.id)), None)
      networkControllerRef ! SendToNetwork(msg, Broadcast)
    }

  private def viewHolderEvents: Receive = {
    case FailedTransaction(tx, throwable, source) =>
    //todo: ban source peer?
    case FailedModification(mod, throwable, source) =>
    //todo: ban source peer?

    case SuccessfulTransaction(tx, source) => sendModifierIfLocal(tx, source)
    case SuccessfulModification(mod, source) => sendModifierIfLocal(mod, source)
  }

  private def getLocalSyncInfo: Receive = {
    case GetLocalSyncInfo =>
      viewHolderRef ! NodeViewHolder.GetSyncInfo
  }

  //sending out sync message to a random peer
  private def syncSend: Receive = {
    case CurrentSyncInfo(syncInfo: SI) =>
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToRandom)
  }


  //sync info is coming from another node
  private def processSync: Receive = {
    case DataFromPeer(spec, syncData: SI, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>

      viewHolderRef ! OtherNodeSyncingInfo(remote, syncData)
  }

  //view holder is telling other node status
  private def processSyncStatus: Receive = {
    case OtherNodeSyncingStatus(remote, status, remoteSyncInfo, localSyncInfo: SI, extOpt) =>
      if (!remoteSyncInfo.answer) {
        networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(localSyncInfo), None), SendToRandom)
      }

      val seniorsBefore = seniors.size

      val remoteHost = remote.socketAddress.getAddress.getHostAddress

      seniors.remove(remoteHost)
      juniors.remove(remoteHost)
      equals.remove(remoteHost)

      status match {
        case Nonsense =>
          log.warn("Got nonsense")

        case Older =>
          seniors.add(remoteHost)

        case Younger =>
          juniors.add(remoteHost)
          assert(extOpt.isDefined)
          val ext = extOpt.get
          ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
            case (mid, mods) =>
              networkControllerRef ! SendToNetwork(Message(InvSpec, Right(mid -> mods), None), SendToPeer(remote))
          }
        case Equal =>
          equals.add(remoteHost)
      }

      val seniorsAfter = seniors.size

      if (seniorsBefore > 0 && seniorsAfter == 0){
        localInterfaceRef ! LocalInterface.NoBetterNeighbour
      }

      if (seniorsBefore == 0 && seniorsAfter > 0){
        localInterfaceRef ! LocalInterface.BetterNeighbourAppeared
      }
  }

  //object ids coming from other node
  private def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.messageCode =>

      viewHolderRef ! CompareViews(remote, invData._1, invData._2)
  }

  //other node asking for objects by their ids
  private def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.messageCode =>

      viewHolderRef ! GetLocalObjects(remote, invData._1, invData._2)
  }

  //other node is sending objects
  private def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>

      val typeId = data._1
      val modifiers = data._2

      val askedIds = asked.getOrElse(typeId, mutable.Set())

      log.info(s"Got modifiers with ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      log.info(s"Asked ids ${data._2.keySet.map(Base58.encode).mkString(",")}")

      val fm = modifiers.flatMap{case(mid, mod) =>
        if(askedIds.exists(id => id sameElements mid)){
          askedIds.retain(id => !(id sameElements mid))
          Some(mod)
        } else {
          None
          //todo: remote peer has sent some object not requested -> ban?
        }
      }.toSeq

      asked.put(typeId, askedIds)
      val msg = ModifiersFromRemote(remote, data._1, fm)
      viewHolderRef ! msg
  }

  //local node sending object ids to remote
  private def requestFromLocal: Receive = {
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>

      if (modifierIds.nonEmpty) {
        val msg = Message(RequestModifierSpec, Right(modifierTypeId -> modifierIds), None)
        peer.handlerRef ! msg
      }
      val newIds = asked.getOrElse(modifierTypeId, mutable.Set()) ++ modifierIds
      asked.put(modifierTypeId, newIds)
  }

  //local node sending out objects requested to remote
  private def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, typeId, modifiers: Seq[NodeViewModifier]) =>
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
      viewHolderEvents orElse {
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

  case class OtherNodeSyncingInfo[SI <: SyncInfo](peer: ConnectedPeer, syncInfo: SI)

}
