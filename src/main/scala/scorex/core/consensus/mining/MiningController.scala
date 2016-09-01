package scorex.core.consensus.mining

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.NodeViewHolder
import scorex.core.consensus.mining.MiningController._
import scorex.core.consensus.mining.Miner._
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Success

class MiningController[P <: Proposition, TX <: Transaction[P, TX]]
(settings: Settings,
 historySynchronizer: ActorRef,
 viewHolder: NodeViewHolder[P, TX]
) extends Actor
  with ScorexLogging {

  val threads = settings.mininigThreads
  val FailedGenerationDelay: FiniteDuration = Math.max(10, settings.blockGenerationDelay.toSeconds).seconds
  implicit val timeout = Timeout(FailedGenerationDelay)

  var workers: Seq[ActorRef] = Seq.empty

  context.system.scheduler.schedule(FailedGenerationDelay, FailedGenerationDelay, self, CheckWorkers)


  override def receive: Receive = syncing

  def syncing: Receive = {
    case StartGeneration =>
      log.info("StartGeneration")
      workers.foreach(w => w ! GuessABlock)
      context.become(generating)

    case GetStatus =>
      sender() ! Syncing.name

    case CheckWorkers =>
      log.info(s"Check miners: $workers vs ${context.children}")
      workers.foreach(w => w ! Stop)
      context.children.foreach(w => w ! Stop)

    case StopGeneration =>

    case m => log.warn(s"Unhandled $m in Syncing")
  }

  def generating: Receive = {
    case StopGeneration =>
      log.info(s"StopGeneration")
      workers.foreach(w => w ! Stop)
      context.become(syncing)

    case Terminated(worker) =>
      log.info(s"Terminated $worker")
      workers = workers.filter(w => w != worker)

    case GetStatus =>
      sender() ! Generating.name

    case CheckWorkers =>
      log.info(s"Check $workers")
      val incTime =
        workers.foreach { w =>
          (w ? GetLastGenerationTime) onComplete {
            case Success(LastGenerationTime(t)) if System.currentTimeMillis() - t < FailedGenerationDelay.toMillis =>
              log.info(s"Miner $w works fine, last try was ${System.currentTimeMillis() - t} millis ago")
            case Success(LastGenerationTime(t)) if System.currentTimeMillis() - t > FailedGenerationDelay.toMillis =>
              log.warn(s"Miner $w don't generate blocks")
              w ! GuessABlock
            case m =>
              log.warn(s"Restart miner $w: $m")
              w ! Stop
          }
        }
      if (threads - workers.size > 0) workers = workers ++ newWorkers(threads - workers.size)

    case m => log.info(s"Unhandled $m in Generating")
  }

  def newWorkers(count: Int): Seq[ActorRef] = (1 to count).map { i =>
    context.watch(context.actorOf(Props(classOf[Miner[P, TX]],
      settings,
      historySynchronizer,
      wallet), s"Worker-${System.currentTimeMillis()}-$i"))
  }
}

object MiningController {

  sealed trait Status {
    val name: String
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object Generating extends Status {
    override val name = "generating"
  }

  case object GetStatus

  case object StartGeneration

  case object StopGeneration

  case object CheckWorkers
}
