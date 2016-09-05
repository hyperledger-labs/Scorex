package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.RequestFromLocal
import scorex.core.network.message._
import scorex.core.transaction.NodeStateModifier._
import scorex.core.transaction.{NodeStateModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition

import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._


class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P, TX]](networkControllerRef: ActorRef) extends Actor {

  import NodeViewSynchronizer._
  import scorex.core.transaction.NodeStateModifier._

  val messageSpecs: Seq[MessageSpec[_]]

  //asked from other nodes
  val asked: Map[ModifierTypeId, mutable.Buffer[ModifierId]]

  private var sessionId = 0L
  private var sessionPeerOpt: Option[ConnectedPeer] = None

  val viewHolderRef: ActorRef

  override def preStart: Unit = {
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
  }

  override def receive: Receive =
    processInv orElse
      processModifiersReq orElse
      requestFromLocal orElse
      responseFromLocal

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
  }

  //local node sending out objects requested to remote
  def responseFromLocal: Receive = {
    case ResponseFromLocal(sid, typeId, modifiers) =>
      if (sid == sessionId && modifiers.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>
          val msg = Message(new ModifiersSpec, Right(typeId -> modifiers), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
  }
}

object NodeViewSynchronizer {

  case class CompareViews(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class GetLocalObjects(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class ResponseFromLocal[M <: NodeStateModifier](sid: Long, modifierTypeId: ModifierTypeId, localObjects: Seq[M])
}
