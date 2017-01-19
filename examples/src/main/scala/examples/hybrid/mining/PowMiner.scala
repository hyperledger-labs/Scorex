package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import examples.hybrid.blocks.{HybridBlock, PowBlock, PowBlockCompanion, PowBlockHeader}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.util.Cancellable
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.block.Block.BlockId
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random

/**
  * A controller for PoW mining
  * currently it is starting to work on getting a (PoW; PoS) block references
  * and stops on a new PoW block found (when PoS ref is unknown)
  */
class PowMiner(viewHolderRef: ActorRef, settings: MiningSettings) extends Actor with ScorexLogging {

  import PowMiner._

  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false

  override def preStart(): Unit = {
    //todo: check for a last block
    if (settings.offlineGeneration) {
      context.system.scheduler.scheduleOnce(1.second)(self ! StartMining)
    }
  }

  override def receive: Receive = {
    case StartMining =>
      mining = true
      self ! MineBlock

    case MineBlock =>
      if (mining) {
        log.info("Mining of previous PoW block stopped")
        cancellableOpt.foreach(_.cancel()) //todo: check status

        context.system.scheduler.scheduleOnce(50.millis) {
          if (cancellableOpt.forall(_.status.isCancelled)) viewHolderRef ! GetCurrentView
          else self ! StartMining
        }
      }

    case CurrentView(h: HybridHistory, s: HBoxStoredState, w: HWallet, m: HMemPool) =>

      if (!cancellableOpt.forall(_.status.isCancelled)) {
        log.warn("Trying to run miner when the old one is still running")
      } else {
        val difficulty = h.powDifficulty

        val (parentId, prevPosId, brothers) = if (!h.pairCompleted) {
          //brother
          log.info(s"Starting brother mining for ${Base58.encode(h.bestPowBlock.parentId)}:${Base58.encode(h.bestPowBlock.prevPosId)}")
          val bs = h.bestPowBlock.brothers :+ h.bestPowBlock.header
          (h.bestPowBlock.parentId, h.bestPowBlock.prevPosId, bs)
        } else {
          log.info(s"Starting new block mining for ${Base58.encode(h.bestPowId)}:${Base58.encode(h.bestPosId)}")
          (h.bestPowId, h.bestPosId, Seq()) //new step
        }

        val p = Promise[Option[PowBlock]]()
        cancellableOpt = Some(Cancellable.run() { status =>
          Future {
            var foundBlock: Option[PowBlock] = None
            var attemps = 0

            while (status.nonCancelled && foundBlock.isEmpty) {
              foundBlock = powIteration(parentId, prevPosId, brothers, difficulty, settings, w.publicKeys.head)
              attemps = attemps + 1
              if (attemps % 100 == 99) {
                log.debug(s"100 hashes tried, difficulty is $difficulty")
              }
            }
            p.success(foundBlock)
          }
        })

        p.future.onComplete { toBlock =>
          toBlock.getOrElse(None).foreach { block =>
            log.debug(s"Locally generated PoW block: $block with difficulty $difficulty")
            self ! block
          }
        }
      }


    case b: PowBlock =>
      cancellableOpt.foreach(_.cancel())
      viewHolderRef ! LocallyGeneratedModifier[PublicKey25519Proposition, SimpleBoxTransaction, HybridBlock](b)

    case StopMining =>
      mining = false

    case a: Any =>
      log.warn(s"Strange input: $a")
  }
}

object PowMiner extends App {
  lazy val HashesPerSecond = 20

  case object StartMining

  case object StopMining

  case object MineBlock

  def powIteration(parentId: BlockId,
                   prevPosId: BlockId,
                   brothers: Seq[PowBlockHeader],
                   difficulty: BigInt,
                   settings: MiningSettings,
                   proposition: PublicKey25519Proposition,
                   hashesPerSecond: Int = HashesPerSecond
                  ): Option[PowBlock] = {
    val nonce = Random.nextLong()

    val ts = System.currentTimeMillis()

    val bHash = if (brothers.isEmpty) Array.fill(32)(0: Byte)
    else FastCryptographicHash(PowBlockCompanion.brotherBytes(brothers))

    val b = PowBlock(parentId, prevPosId, ts, nonce, brothers.size, bHash, proposition, brothers)

    val foundBlock =
      if (b.correctWork(difficulty, settings)) {
        Some(b)
      } else {
        None
      }
    Thread.sleep(1000 / hashesPerSecond)
    foundBlock
  }
}