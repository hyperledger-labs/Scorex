package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.transaction.NodeViewModifier._
import scorex.core.transaction.{NodeViewModifier, NodeViewModifierCompanion, Transaction}
import scorex.core.transaction.box.proposition.Proposition

import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._

import scala.collection.concurrent.TrieMap


class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P, TX]]
(networkControllerRef: ActorRef, viewHolderRef: ActorRef) extends Actor {

  import NodeViewSynchronizer._
  import scorex.core.transaction.NodeViewModifier._

  //asked from other nodes
  private val asked = TrieMap[ModifierTypeId, mutable.Buffer[ModifierId]]()
  private var sessionId = 0L
  private var sessionPeerOpt: Option[ConnectedPeer] = None

  override def preStart(): Unit = {
    val messageSpecs = Seq(InvSpec, RequestModifierSpec, ModifiersSpec)
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
  }

  override def receive: Receive =
    processInv orElse
      processModifiersReq orElse
      requestFromLocal orElse
      responseFromLocal

  //object ids coming from other node
  def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.messageCode =>

      viewHolderRef ! CompareViews(sessionId, invData._1, invData._2)
      sessionId = sessionId + 1
      sessionPeerOpt = Some(remote)
  }

  //other node asking for objects by their ids
  def processModifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.messageCode =>

      viewHolderRef ! GetLocalObjects(sessionId, invData._1, invData._2)
  }

  //other node is sending objects
  def processModifiers: Receive = {
    case DataFromPeer(spec, data: (ModifierTypeId, Seq[Array[Byte]])@unchecked, _) if spec.messageCode == ModifiersSpec.messageCode =>

      viewHolderRef ! ModifiersFromRemote(sessionId, data._1, data._2)
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
    case ResponseFromLocal(sid, typeId, modifiers: Seq[NodeViewModifier]) =>
      if (sid == sessionId && modifiers.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>

          //todo: asInstanceOf, convert to bytes in NodeViewHolder?
          val c = modifiers.head.companion.asInstanceOf[NodeViewModifierCompanion[NodeViewModifier]]
          val modType = modifiers.head.modifierTypeId

          val m = modType -> c.bytes(modifiers)
          val msg = Message(ModifiersSpec, Right(m), None)
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

  case class ResponseFromLocal[M <: NodeViewModifier](sid: Long, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

  case class ModifiersFromRemote(sid: Long, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

}
