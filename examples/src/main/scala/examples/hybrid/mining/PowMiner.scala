package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PowBlock, PowBlockCompanion}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.util.Cancellable
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait MiningSettings extends Settings {
  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)
}

/**
  * A controller for PoW mining
  * currently it is starting to work on getting a (PoW; PoS) block references
  * and stops on a new PoW block found (when PoS ref is unknown)
  */
class PowMiner(viewHolderRef: ActorRef, miningSettings: MiningSettings) extends Actor with ScorexLogging {

  import PowMiner._

  private var cancellableOpt: Option[Cancellable] = None
  private var mining = false

  override def preStart(): Unit = {
    //todo: check for a last block
    if (miningSettings.offlineGeneration) {
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

        val difficulty = h.powDifficulty

        val (parentId, prevPosId, brothers) = if (!h.pairCompleted) {
          //brother
          log.info(s"Starting brother mining for ${Base58.encode(h.bestPowBlock.parentId)}:${Base58.encode(h.bestPowBlock.parentId)}")
          val bs = h.bestPowBlock.brothers :+ h.bestPowBlock.header
          (h.bestPowBlock.parentId, h.bestPowBlock.parentId, bs)
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
              val nonce = Random.nextLong()

              val ts = System.currentTimeMillis()

              val bHash = if (brothers.isEmpty) Array.fill(32)(0: Byte)
              else FastCryptographicHash(PowBlockCompanion.brotherBytes(brothers))

              val b = PowBlock(parentId, prevPosId, ts, nonce, brothers.size, bHash, brothers)

              foundBlock =
                if (b.correctWork(difficulty)) {
                  Some(b)
                } else {
                  attemps = attemps + 1
                  if (attemps % 100 == 99) {
                    println(s"100 hashes tried, difficulty is $difficulty")
                  }
                  None
                }
              Thread.sleep(5) //200 hashes per second per node
            }
            p.success(foundBlock)
          }
        })

        p.future.onComplete { toBlock =>
          toBlock.getOrElse(None).foreach(block => self ! block)
        }


    case b: PowBlock =>
      println(s"locally generated PoW block: $b")
      cancellableOpt.foreach(_.cancel())
      viewHolderRef ! LocallyGeneratedModifier[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier](b)

    case StopMining =>
      mining = false

    case a: Any =>
      log.warn(s"Strange input: $a")
  }
}

object PowMiner extends App {
  lazy val BlockDelay = 6.seconds.toMillis

  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("5")

  lazy val GenesisParentId = Array.fill(32)(1: Byte)

  lazy val MaxBlockSize = 100000

  case object StartMining

  case object StopMining

  case object MineBlock
}