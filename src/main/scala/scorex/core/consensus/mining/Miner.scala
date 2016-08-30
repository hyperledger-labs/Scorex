package scorex.core.consensus.mining

import akka.actor.{Actor, ActorRef}
import scorex.core.block.{Block, ConsensusData}
import scorex.core.consensus.mining.Miner._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.core.transaction.Transaction
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class Miner[P <: Proposition, TX <: Transaction[P, TX]]
(settings: Settings,
 historySynchronizer: ActorRef,
 wallet: Wallet[P, TX]
)
  extends Actor with ScorexLogging {

  // BlockGenerator is trying to generate a new block every $blockGenerationDelay. Should be 0 for PoW consensus model.
  val blockGenerationDelay = settings.blockGenerationDelay
  val BlockGenerationTimeLimit = 5.seconds

  var lastTryTime = 0L
  var stopped = false

  private def scheduleAGuess(delay: Option[FiniteDuration] = None): Unit =
    context.system.scheduler.scheduleOnce(delay.getOrElse(blockGenerationDelay), self, GuessABlock)

  scheduleAGuess(Some(0.millis))

  override def receive: Receive = {
    case GuessABlock =>
      stopped = false
      if (System.currentTimeMillis() - lastTryTime >= blockGenerationDelay.toMillis) tryToGenerateABlock()

    case GetLastGenerationTime =>
      sender ! LastGenerationTime(lastTryTime)

    case Stop =>
      stopped = true
  }

  def tryToGenerateABlock(): Unit = Try {

    lastTryTime = System.currentTimeMillis()
    if (blockGenerationDelay > 500.milliseconds) log.info("Trying to generate a new block")
    val timestamp = NetworkTime.time()
    val tData = transactionalModule.generateTdata(timestamp)
    val cFuture = consensusModule.generateCdata(wallet, timestamp, tData.id)
    Await.result(cFuture, BlockGenerationTimeLimit) match {
      case Some(cData) =>
        val block = Block(timestamp, cData, tData)
        historySynchronizer ! block
      case _ =>
    }

    if (!stopped) scheduleAGuess()
  }.recoverWith {
    case ex =>
      log.error("Failed to generate new block", ex)
      Failure(ex)
  }
}

object Miner {

  case object Stop

  case object GuessABlock

  case object GetLastGenerationTime

  case class LastGenerationTime(time: Long)

}
