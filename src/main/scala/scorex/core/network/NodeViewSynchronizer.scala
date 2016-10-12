package scorex.core.network

import akka.actor.{Actor, ActorRef}
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{FailedModification, FailedTransaction, Subscribe}
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.transaction.NodeViewModifier._
import scorex.core.transaction.{NodeViewModifier, NodeViewModifierCompanion, Transaction}
import scorex.core.transaction.box.proposition.Proposition

import scala.collection.mutable
import scorex.core.network.message.BasicMsgDataTypes._

/**
  * A middle layer between a node view holder(NodeViewHolder) and a network
  * //todo: also a local updater?
  *
  * @param networkControllerRef
  * @param viewHolderRef
  * @tparam P
  * @tparam TX
  */
class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P]]
(networkControllerRef: ActorRef, viewHolderRef: ActorRef) extends Actor {

  import NodeViewSynchronizer._
  import scorex.core.transaction.NodeViewModifier._

  //asked from other nodes
  //todo: clean old entries in asked
  private val asked = mutable.Map[ModifierTypeId, mutable.Set[ModifierId]]()

  private var sessionId = 0L
  private var sessionPeerOpt: Option[ConnectedPeer] = None

  override def preStart(): Unit = {
    //register as a handler for some types of messages
    val messageSpecs = Seq(InvSpec, RequestModifierSpec, ModifiersSpec)
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)

    //subscribe for failed transaction,
    val events = Seq(NodeViewHolder.EventType.FailedTransaction, NodeViewHolder.EventType.FailedPersistentModifier)
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case FailedTransaction(tx, throwable, source) =>
    //todo: ban source peer?
    case FailedModification(mod, throwable, source) =>
    //todo: ban source peer?
  }

  //object ids coming from other node
  private def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.messageCode =>

      viewHolderRef ! CompareViews(sessionId, invData._1, invData._2)
      sessionId = sessionId + 1
      sessionPeerOpt = Some(remote)
  }

  //other node asking for objects by their ids
  private def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.messageCode =>

      viewHolderRef ! GetLocalObjects(sessionId, invData._1, invData._2)
  }

  //other node is sending objects
  private def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.messageCode =>

      val typeId = data._1
      val modifiers = data._2

      val modIds = modifiers.keySet

      val askedIds = asked.getOrElse(typeId, mutable.Set())
      val filteredIds = askedIds.diff(modIds)

      if (askedIds.size - modIds.size == filteredIds.size) {
        val msg = ModifiersFromRemote(sessionId, data._1, modifiers.valuesIterator.toSeq, remote)
        viewHolderRef ! msg
        asked.put(typeId, filteredIds)
      } else {
        //peer has sent some object not requested -> ban!
        //todo: ban a peer
      }
  }

  //local node sending object ids to remote
  private def requestFromLocal: Receive = {
    case RequestFromLocal(sid, modifierTypeId, modifierIds) =>
      if (sid == sessionId && modifierIds.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>
          val msg = Message(RequestModifierSpec, Right(modifierTypeId -> modifierIds), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
      val newids = asked.getOrElse(modifierTypeId, mutable.Set()) ++ modifierIds
      asked.put(modifierTypeId, newids)
  }

  //local node sending out objects requested to remote
  private def responseFromLocal: Receive = {
    case ResponseFromLocal(sid, typeId, modifiers: Seq[NodeViewModifier]) =>
      if (sid == sessionId && modifiers.nonEmpty) {
        sessionPeerOpt.foreach { sessionPeer =>

          //todo: asInstanceOf, convert to bytes in NodeViewHolder?
          val c = modifiers.head.companion.asInstanceOf[NodeViewModifierCompanion[NodeViewModifier]]
          val modType = modifiers.head.modifierTypeId

          val m = modType -> modifiers.map(m => m.id -> c.bytes(m)).toMap
          val msg = Message(ModifiersSpec, Right(m), None)
          sessionPeer.handlerRef ! msg
        }
      }
      sessionPeerOpt = None
  }

  override def receive: Receive =
    processInv orElse
      modifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse
      modifiersFromRemote orElse
      viewHolderEvents
}

object NodeViewSynchronizer {

  case class CompareViews(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class GetLocalObjects(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class RequestFromLocal(sid: Long, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class ResponseFromLocal[M <: NodeViewModifier](sid: Long, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

  case class ModifiersFromRemote(sid: Long, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]], remote: ConnectedPeer)

}
