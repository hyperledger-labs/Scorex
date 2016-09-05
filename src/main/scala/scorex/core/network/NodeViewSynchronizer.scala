package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.transaction.NodeStateModifier._
import scorex.core.transaction.{NodeStateModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition

import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._

import scala.collection.concurrent.TrieMap


class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P, TX]]
(networkControllerRef: ActorRef, viewHolderRef: ActorRef) extends Actor {

  import NodeViewSynchronizer._
  import scorex.core.transaction.NodeStateModifier._

  //asked from other nodes
  private val asked = TrieMap[ModifierTypeId, mutable.Buffer[ModifierId]]()

  private var modifiersSpecs = Map[ModifierTypeId, ModifiersSpec[_ <: NodeStateModifier]]()

  private var sessionId = 0L
  private var sessionPeerOpt: Option[ConnectedPeer] = None

  override def receive: Receive =
    processInv orElse
      processModifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse {
      case HistoryModifiersSpecs(mSpecs) =>
        val messageSpecs = Seq(InvSpec, RequestModifierSpec) ++ mSpecs
        networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
        modifiersSpecs = mSpecs.map(ms => ms.modTypeId -> ms).toMap
    }

  //object ids coming from other node
  def processInv: Receive = {
    case DataFromPeer(msgId, invData: InvData@unchecked, remote) if msgId == InvSpec.messageCode =>
      viewHolderRef ! CompareViews(sessionId, invData._1, invData._2)
      sessionId = sessionId + 1
      sessionPeerOpt = Some(remote)
  }

  //other node asking for objects by their ids
  def processModifiersReq: Receive = {
    case DataFromPeer(msgId, invData: InvData@unchecked, remote) if msgId == RequestModifierSpec.messageCode =>
      viewHolderRef ! GetLocalObjects(sessionId, invData._1, invData._2)
  }

  //other node is sending objects
  def processModifiers: Receive = {
    case DataFromPeer(msgId, mod, _) if msgId == ModifiersSpec.messageCode =>
      ???
  }

  //local node sending object ids to remote
  def requestFromLocal: Receive = {
    case RequestFromLocal(sid, modifierTypeId, modifierIds) =>
      if (sid == sessionId && modifierIds.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>
          val msg = Message(RequestModifierSpec, Right(modifierTypeId -> modifierIds), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
      val newids = asked.getOrElse(modifierTypeId, mutable.Buffer()) ++ modifierIds
      asked.put(modifierTypeId, newids)
  }

  //local node sending out objects requested to remote
  def responseFromLocal: Receive = {
    case ResponseFromLocal(sid, typeId, modifiers) =>
      if (sid == sessionId && modifiers.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>
          val msg = Message(modifiersSpecs(typeId), Right(modifiers), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
  }
}

object NodeViewSynchronizer {

  case object Init

  case class HistoryModifiersSpecs(specs: Seq[ModifiersSpec[_ <: NodeStateModifier]])

  case class CompareViews(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class GetLocalObjects(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class ResponseFromLocal[M <: NodeStateModifier](sid: Long, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

}
