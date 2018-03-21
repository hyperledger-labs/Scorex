package examples.hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.mining.HybridMiningSettings
import scorex.core.{LocalInterface, ModifierId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class HLocalInterface(override val viewHolderRef: ActorRef,
                      powMinerRef: ActorRef,
                      posForgerRef: ActorRef,
                      minerSettings: HybridMiningSettings)
  extends LocalInterface[PublicKey25519Proposition, SimpleBoxTransaction, HybridBlock] {

  import examples.hybrid.mining.PosForger.ReceivableMessages.{StartForging, StopForging}
  import examples.hybrid.mining.PowMiner.ReceivableMessages.{MineBlock, StartMining, StopMining}

  private var block = false

  override protected def onStartingPersistentModifierApplication(pmod: HybridBlock): Unit = {}

  override protected def onFailedTransaction(tx: SimpleBoxTransaction): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: HybridBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: SimpleBoxTransaction): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: HybridBlock): Unit = {}

  override protected def onSemanticallyFailedModification(mod: HybridBlock): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {
    log.error("Too deep rollback occurred!")
  }

  //stop PoW miner and start PoS forger if PoW block comes
  //stop PoW forger and start PoW miner if PoS block comes
  override protected def onSemanticallySuccessfulModification(mod: HybridBlock): Unit = {
    if (!block) {
      mod match {
        case _: PowBlock =>
          posForgerRef ! StartForging
          powMinerRef ! MineBlock

        case sb: PosBlock =>
          if (!(sb.parentId sameElements minerSettings.GenesisParentId)) {
            posForgerRef ! StopForging
            powMinerRef ! StartMining
          }
      }
    }
  }

  override protected def onNoBetterNeighbour(): Unit = {
    powMinerRef ! StartMining
    posForgerRef ! StartForging
    block = false
  }

  override protected def onBetterNeighbourAppeared(): Unit = {
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
