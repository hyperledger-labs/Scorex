package examples.curvepos.forging

import akka.actor.{Actor, ActorRef}
import examples.curvepos.SimpleBlockchain
import examples.curvepos.transaction.{SimpleBlock, SimpleMemPool, SimpleState, SimpleWallet}
import scorex.core.NodeViewHolder.GetCurrentView
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.NetworkTime

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait ForgerSettings extends Settings {
  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)
}

class Forger(viewHolderRef: ActorRef, forgerSettings: ForgerSettings) extends Actor {

  import Forger._

  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  //set to true for initial generator
  private var forging = forgerSettings.offlineGeneration

  private val hash = FastCryptographicHash


  val InterBlocksDelay = 15 //in seconds
  val blockGenerationDelay = 500 //in millis

  override def preStart(): Unit = {
    if (forging) context.system.scheduler.scheduleOnce(1.second)(self ! Forge)
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  private def calcBaseTarget(lastBlock: SimpleBlock,
                             currentTime: Long): Long = {
    val eta = currentTime - lastBlock.timestamp
    val prevBt = BigInt(lastBlock.baseTarget)
    val t0 = bounded(prevBt * eta / InterBlocksDelay, prevBt / 2, prevBt * 2)
    bounded(t0, 1, Long.MaxValue).toLong
  }

  protected def calcTarget(lastBlock: SimpleBlock,
                           state: SimpleState,
                           generator: PublicKey25519Proposition): BigInt = {
    val eta = (NetworkTime.time() - lastBlock.timestamp) / 1000 //in seconds
    val balance = state.boxOf(generator).headOption.map(_.value).getOrElse(0L)
    BigInt(lastBlock.baseTarget) * eta * balance
  }

  private def calcGeneratorSignature(lastBlock: SimpleBlock, generator: PublicKey25519Proposition) =
    hash(lastBlock.generationSignature ++ generator.pubKeyBytes)

  private def calcHit(lastBlock: SimpleBlock, generator: PublicKey25519Proposition): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlock, generator).take(8))

  override def receive: Receive = {
    case StartMining =>
      forging = true
      context.system.scheduler.scheduleOnce(500.millisecond)(self ! Forge)

    case StopMining =>
      forging = false

    case t:(SimpleBlockchain, SimpleState, SimpleWallet, SimpleMemPool) =>
      val currentState = t._2
      val lastBlock = t._1.lastBlock
      val memPool = t._4



      val toInclude = currentState.filterValid(memPool.drain(TransactionsInBlock)._1.toSeq)


      val gs = lastBlock.generationSignature



    case Forge =>
      viewHolderRef ! GetCurrentView
      context.system.scheduler.scheduleOnce(1.second)(self ! Forge)

  }
}

object Forger {

  case object StartMining

  case object StopMining

  case object Forge

}