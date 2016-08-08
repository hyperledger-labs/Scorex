package scorex.network

import akka.actor.ActorRef
import scorex.NodeStateHolder
import scorex.block.ConsensusData.BlockId
import scorex.block.{ConsensusData, TransactionalData}
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.{GetBlockSpec, GetSignaturesSpec, Message, SignaturesSpec}
import scorex.settings.Settings
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition
import scorex.utils.ScorexLogging

class HistoryReplier[P <: Proposition, TX <: Transaction[P, TX], TD <: TransactionalData[TX], CD <: ConsensusData]
(settings: Settings, stateHolder: NodeStateHolder[P, TX, TD, CD], networkControllerRef: ActorRef) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(GetSignaturesSpec, GetBlockSpec)

  private def history() = stateHolder.stableState._2

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
        val msg = Message(BlockMessageSpec, Right(b), None)
        val ss = SendToChosen(Seq(remote))
        networkControllerRef ! SendToNetwork(msg, ss)
      }

    //the signal to initialize
    case Unit =>
  }
}