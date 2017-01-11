package examples.hybrid.history

import akka.actor.{Actor, ActorRef}
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.util.FileFunctions
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView, Subscribe, SuccessfulModification}
import scorex.core.NodeViewModifier._
import scorex.core.network.ConnectedPeer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.collection.mutable

class StatsLogger(logDirOpt: Option[String], viewHolderRef: ActorRef) extends Actor {

  private val modifiersCache = mutable.ArrayBuffer[ModifierId]()

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulPersistentModifier))
  }

  override def receive: Receive = {
    case sm: SuccessfulModification[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier] =>
      sm.modifier match {
        case b: PosBlock =>
        case b: PowBlock =>
          modifiersCache += b.id
          viewHolderRef ! GetCurrentView
      }
    case CurrentView(h: HybridHistory, s: HBoxStoredState, w: HWallet, m: HMemPool) =>
//TODO      logOrphanCount(h.orphanCountVar.get(), h.bestChainScore)
      //TODO logForkDepth

  }

  def logForkDepth(forkLength: Int, bestChainScore: Long): Unit = {
    logDirOpt.foreach { logDir =>
      FileFunctions.append(logDir + "/forkdepth.csv", s"$forkLength, $bestChainScore")
    }
  }

  def logOrphanCount(orphanCount: Long, bestChainScore: Long): Unit = {
    logDirOpt.foreach { logDir =>
      val record = s"$orphanCount, $bestChainScore"
      FileFunctions.append(logDir + "/orphans.csv", record)
    }
  }
}
