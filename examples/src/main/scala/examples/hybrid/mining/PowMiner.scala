package examples.hybrid.mining

import akka.actor.Actor
import examples.hybrid.blocks.PowBlock
import examples.hybrid.util.Cancellable
import scorex.core.block.Block._
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.Random
import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A controller for PoW mining
  * currently it is starting to work on getting a (PoW; PoS) block references
  * and stops on a new PoW block found (when PoS ref is unknown)
  */
class PowMiner extends Actor with ScorexLogging {

  import PowMiner._

  private var cancellableOpt: Option[Cancellable] = None

  override def receive: Receive = {
    case StartMining(parentId: BlockId, prevPosId: BlockId) =>
      log.info(s"Starting mining for ${Base58.encode(parentId)}:${Base58.encode(prevPosId)}")

      val p = Promise[Option[PowBlock]]()
      cancellableOpt = Some(Cancellable.run() { status =>
        Future {
          var foundBlock: Option[PowBlock] = None

          while (status.nonCancelled && foundBlock.isEmpty) {
            val nonce = Random.nextLong()

            val ts = System.currentTimeMillis()
            val b = PowBlock(parentId, prevPosId, ts, nonce)

            foundBlock =
              if (b.correctWork) {
                Some(b)
              } else {
                println(s"tried: $nonce $ts")
                None
              }
          }
          p.success(foundBlock)
        }
      })

      p.future.onComplete { toBlock =>
        toBlock.getOrElse(None).foreach(block => self ! block)
      }

    case b: PowBlock =>
      println(s"locally found block: $b")

    case StopMining =>
      log.info("Mining stopped")
      cancellableOpt.foreach(_.cancel())

    case a: Any =>
      log.warn(s"Strange input: $a")
  }
}

object PowMiner extends App {
  lazy val Difficulty = BigInt(848376755153961272L).pow(4)

  lazy val GenesisParentId = Array.fill(32)(1: Byte)

  lazy val MaxBlockSize = 100000

  case class StartMining(parentId: BlockId, prevPosId: BlockId)

  case object StopMining

}