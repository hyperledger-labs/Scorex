package examples.hybrid

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.hybrid.blocks.{PosBlock, PowBlock}
import examples.hybrid.mining.HybridMiningSettings
import scorex.core.network.NodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour, NodeViewSynchronizerEvent}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{NodeViewHolderEvent, RollbackFailed, SemanticallySuccessfulModifier}
import scorex.core.utils.ScorexLogging

class HLocalInterface(viewHolderRef: ActorRef,
                      powMinerRef: ActorRef,
                      posForgerRef: ActorRef,
                      minerSettings: HybridMiningSettings) extends Actor with ScorexLogging {

  import examples.hybrid.mining.PosForger.ReceivableMessages.{StartForging, StopForging}
  import examples.hybrid.mining.PowMiner.ReceivableMessages.{MineBlock, StartMining, StopMining}

  private var block = false


  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewHolderEvent])
    context.system.eventStream.subscribe(self, classOf[NodeViewSynchronizerEvent])
  }

  override def receive: Receive = {
    case RollbackFailed => log.error("Too deep rollback occurred!")

    //stop PoW miner and start PoS forger if PoW block comes
    //stop PoW forger and start PoW miner if PoS block comes
    case sems: SemanticallySuccessfulModifier[_] =>
      if (!block) {
        sems.modifier match {
          case wb: PowBlock =>
            posForgerRef ! StartForging
            powMinerRef ! MineBlock

          case sb: PosBlock =>
            if (!(sb.parentId == minerSettings.GenesisParentId)) {
              posForgerRef ! StopForging
              powMinerRef ! StartMining
            }
        }
      }

    case NoBetterNeighbour =>
      powMinerRef ! StartMining
      posForgerRef ! StartForging
      block = false

    case BetterNeighbourAppeared =>
      powMinerRef ! StopMining
      posForgerRef ! StopForging
      block = true
  }
}

object HLocalInterfaceRef {
  def props(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            posForgerRef: ActorRef,
            minerSettings: HybridMiningSettings): Props =
    Props(new HLocalInterface(viewHolderRef, powMinerRef, posForgerRef, minerSettings))

  def apply(viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            posForgerRef: ActorRef,
            minerSettings: HybridMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, posForgerRef, minerSettings))

  def apply(name: String, viewHolderRef: ActorRef,
            powMinerRef: ActorRef,
            posForgerRef: ActorRef,
            minerSettings: HybridMiningSettings)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, powMinerRef, posForgerRef, minerSettings), name)
}
