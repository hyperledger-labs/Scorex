package scorex.core.network

import akka.actor.ActorRef
import scorex.core.NodeViewHolder
import scorex.core.block.{Block, ConsensusData}
import scorex.core.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.core.network.message._
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

/*
class HistoryReplier[P <: Proposition, TX <: Transaction[P, TX], B <: Block[P, TX]]
(settings: Settings,
 stateHolder: NodeViewHolder[P, TX],
 val networkControllerRef: ActorRef,
 blockMessageSpec: BlockMessageSpec[P, TX, B]) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(GetSignaturesSpec, GetBlockSpec)

  override def receive: Receive = {

    //todo: check sender and otherSigs type
    case DataFromPeer(msgId, otherSigs: Seq[BlockId]@unchecked, remote)
      if msgId == GetSignaturesSpec.messageCode =>

      log.info(s"Got GetSignaturesMessage with ${otherSigs.length} sigs within")

      otherSigs.exists { parent =>
        val headers = history().lookForward(parent, settings.MaxBlocksChunks)

        if (headers.nonEmpty) {
          val msg = Message(SignaturesSpec, Right(Seq(parent) ++ headers), None)
          val ss = SendToChosen(Seq(remote))
          networkControllerRef ! SendToNetwork(msg, ss)
          true
        } else false
      }

    //todo: check sender?
    case DataFromPeer(msgId, sig: BlockId@unchecked, remote)
      if msgId == GetBlockSpec.messageCode =>

      history().blockById(sig).foreach { b =>
        val msg = Message(blockMessageSpec, Right(b), None)
        val ss = SendToChosen(Seq(remote))
        networkControllerRef ! SendToNetwork(msg, ss)
      }

    //the signal to initialize
    case Unit =>
  }
}*/