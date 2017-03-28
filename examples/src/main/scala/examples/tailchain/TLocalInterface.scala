package examples.tailchain

import akka.actor.ActorRef
import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.mining.PosForger.{StartForging, StopForging}
import examples.hybrid.mining.MiningSettings
import examples.hybrid.mining.PowMiner.{MineBlock, StartMining, StopMining}
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class TLocalInterface(override val viewHolderRef: ActorRef,
                      powMinerRef: ActorRef,
                      posForgerRef: ActorRef,
                      miningSettings: MiningSettings)
  extends LocalInterface[PublicKey25519Proposition, SimpleBoxTransaction, HybridBlock] {

  private var block = false

  override protected def onStartingPersistentModifierApplication(pmod: HybridBlock): Unit = {}

  override protected def onFailedTransaction(tx: SimpleBoxTransaction): Unit = {}

  override protected def onFailedModification(mod: HybridBlock): Unit = {}

  override protected def onSuccessfulTransaction(tx: SimpleBoxTransaction): Unit = {}

  //stop PoW miner and start PoS forger if PoW block comes
  //stop PoW forger and start PoW miner if PoS block comes
  override protected def onSuccessfulModification(mod: HybridBlock): Unit = {
    if (!block) {
      mod match {
        case wb: PowBlock =>
          posForgerRef ! StartForging
          powMinerRef ! MineBlock

        case sb: PosBlock =>
          if (!(sb.parentId sameElements miningSettings.GenesisParentId)) {
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