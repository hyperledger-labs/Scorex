package examples.curvepos.forging

import akka.actor.{Actor, ActorRef}
import examples.curvepos.SimpleBlockchain
import examples.curvepos.transaction._
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetDataFromCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

trait ForgerSettings extends Settings {
  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)
}

class Forger(viewHolderRef: ActorRef, forgerSettings: ForgerSettings) extends Actor with ScorexLogging {

  import Forger._

  //set to true for initial generator
  private var forging = forgerSettings.offlineGeneration

  private val hash = FastCryptographicHash


  //in seconds
  val InterBlocksDelay = 15
  val blockGenerationDelay = 500.millisecond

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
                           boxOpt: Option[PublicKey25519NoncedBox]): BigInt = {
    val eta = (NetworkTime.time() - lastBlock.timestamp) / 1000 //in seconds
    val balance = boxOpt.map(_.value).getOrElse(0L)
    BigInt(lastBlock.baseTarget) * eta * balance
  }

  private def calcGeneratorSignature(lastBlock: SimpleBlock, generator: PublicKey25519Proposition) =
    hash(lastBlock.generationSignature ++ generator.pubKeyBytes)

  private def calcHit(lastBlock: SimpleBlock, generator: PublicKey25519Proposition): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlock, generator).take(8))

  override def receive: Receive = {
    case StartMining =>
      forging = true
      context.system.scheduler.scheduleOnce(blockGenerationDelay)(self ! Forge)

    case StopMining =>
      forging = false

    case info: RequiredForgingInfo =>
      val lastBlock = info.lastBlock
      log.info(s"Trying to generate a new block on top of $lastBlock")
      lazy val toInclude = info.toInclude

      val generatedBlocks = info.gbs.flatMap { gb =>
        val generator = gb._1
        val hit = calcHit(lastBlock, generator)
        val target = calcTarget(lastBlock, gb._2)
        if (hit < target) {
          Try {
            val timestamp = NetworkTime.time()
            val bt = calcBaseTarget(lastBlock, timestamp)
            val secret = gb._3

            val unsigned: SimpleBlock = SimpleBlock(lastBlock.id, timestamp, Array(), bt, generator, toInclude)
            val signature = PrivateKey25519Companion.sign(secret, unsigned.serializer.messageToSign(unsigned))
            val signedBlock = unsigned.copy(generationSignature = signature.signature)
            log.info(s"Generated new block: ${signedBlock.json.noSpaces}")
            LocallyGeneratedModifier[PublicKey25519Proposition, SimpleTransaction, SimpleBlock](signedBlock)
          }.toOption
        } else {
          None
        }
      }
      generatedBlocks.foreach(localModifier => viewHolderRef ! localModifier)
      context.system.scheduler.scheduleOnce(blockGenerationDelay)(self ! Forge)

    case Forge =>
      viewHolderRef ! Forger.getRequiredData
  }
}

object Forger {
  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  val getRequiredData: GetDataFromCurrentView[SimpleBlockchain,
    SimpleState,
    SimpleWallet,
    SimpleMemPool,
    RequiredForgingInfo] = {
    val f: CurrentView[SimpleBlockchain, SimpleState, SimpleWallet, SimpleMemPool] => RequiredForgingInfo = {
      view: CurrentView[SimpleBlockchain, SimpleState, SimpleWallet, SimpleMemPool] =>
        val toInclude = view.state.filterValid(view.pool.take(TransactionsInBlock).toSeq)
        val lastBlock = view.history.lastBlock
        val gbs: Seq[(PublicKey25519Proposition, Option[PublicKey25519NoncedBox], PrivateKey25519)] = {
          view.vault.publicKeys.map { pk =>
            val boxOpt: Option[PublicKey25519NoncedBox] = view.state.boxesOf(pk).headOption
            val secret: PrivateKey25519 = view.vault.secretByPublicImage(pk).get
            (pk, boxOpt, secret)
          }.toSeq
        }
        RequiredForgingInfo(toInclude, lastBlock, gbs)
    }
    GetDataFromCurrentView[SimpleBlockchain,
      SimpleState,
      SimpleWallet,
      SimpleMemPool,
      RequiredForgingInfo](f)
  }

  case class RequiredForgingInfo(toInclude: Seq[SimpleTransaction],
                                 lastBlock: SimpleBlock,
                                 gbs: Seq[(PublicKey25519Proposition, Option[PublicKey25519NoncedBox], PrivateKey25519)])


  case object StartMining

  case object StopMining

  case object Forge

}