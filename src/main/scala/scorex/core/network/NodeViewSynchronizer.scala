package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.RequestFromLocal
import scorex.core.network.message.{InvSpec, Message, MessageSpec, RequestModifierSpec}
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

  override def receive: Receive = {
    case DataFromPeer(msgId, invData: InvData@unchecked, remote) if msgId == InvSpec.messageCode =>
      viewHolderRef ! PartialOpenSurface(sessionId, invData._1, invData._2)
      sessionId = sessionId + 1
      sessionPeerOpt = Some(remote)

    case DataFromPeer(msgId, invData: InvData@unchecked, remote) if msgId == RequestModifierSpec.messageCode =>
      viewHolderRef ! GetContinuation(sessionId, invData._1, invData._2)

    case RequestFromLocal(sid, modifierTypeId, modifierIds) =>
      if (sid == sessionId && modifierIds.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>
          val msg = Message(RequestModifierSpec, Right(modifierTypeId -> modifierIds), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
  }
}

object NodeViewSynchronizer {

  case class PartialOpenSurface(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class GetContinuation(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

}
