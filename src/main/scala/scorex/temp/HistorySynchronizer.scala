package scorex.core.network

import akka.actor.{ActorRef, Props}
import scorex.core.NodeViewHolder
import scorex.core.block._
import scorex.core.consensus.mining.MiningController
import scorex.core.consensus.mining.MiningController._
import scorex.crypto.encode.Base58
import scorex.core.network.NetworkController.DataFromPeer
import scorex.core.network.ScoreObserver.{ConsideredValue, GetScore, UpdateScore}
import scorex.core.network.message._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.core.transaction.{NodeViewModifier$, Transaction}
import scorex.core.utils.{BlockTypeable, ScorexLogging}
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/*
//todo: write tests
class HistorySynchronizer[P <: Proposition, TX <: Transaction[P, TX], B <: Block[P, TX]]
(settings: Settings,
 viewHolder: NodeViewHolder[P, TX],
 val networkControllerRef: ActorRef) extends ViewSynchronizer with ScorexLogging {

  type Id = NodeStateModifier.ModifierId

  import HistorySynchronizer._

  lazy val historyReplier = context.system.actorOf(Props(classOf[HistoryReplier[P, TX, B]], settings, viewHolder,
    networkControllerRef), "HistoryReplier")

  lazy val blockGenerator = context.system.actorOf(Props(classOf[MiningController[P, TX]],
    settings, self, viewHolder), "blockGenerator")

  type B = Block[P, TX]

  private implicit val blockTypeable = new BlockTypeable[P, TX]

  override val messageSpecs = Seq(ScoreMessageSpec, SignaturesSpec, blockMessageSpec)

  private lazy val scoreObserver = context.actorOf(Props(classOf[ScoreObserver], self))

  private val GettingBlockTimeout = settings.historySynchronizerTimeout

  //TODO increase from small number up to maxRollback
  private val blocksToSend = 100

  var lastUpdate = System.currentTimeMillis()

  private def history() = viewHolder.history()

  override def preStart: Unit = {
    super.preStart()
    //todo: make configurable
    context.system.scheduler.schedule(1.second, 2.seconds) {
      val msg = Message(ScoreMessageSpec, Right(history().score()), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToRandom)
    }

    context.system.scheduler.schedule(GettingBlockTimeout, GettingBlockTimeout, self, SelfCheck)
  }

  override def receive: Receive = if (settings.offlineGeneration) gotoSynced() else gotoSyncing()

  def state(status: Status, logic: Receive): Receive =
  //combine specific logic with common for all the states
    logic orElse ({
      case HistorySynchronizer.GetStatus =>
        sender() ! status.name

      //todo: check sender
      case DataFromPeer(msgId, score: BigInt, connectedPeer)
        if msgId == ScoreMessageSpec.messageCode =>

        scoreObserver ! UpdateScore(Some(connectedPeer -> score))

      case ConsideredValue(Some(networkScore: BigInt), witnesses) =>

      case ConsideredValue(None, _) =>
        log.info("Got no score from outer world")
        if (settings.offlineGeneration) gotoSynced() else gotoSyncing()

      case SelfCheck =>
        if (status != Syncing && System.currentTimeMillis() - lastUpdate > GettingBlockTimeout.toMillis) gotoSyncing()

      //the signal to initialize
      case Unit =>
        historyReplier ! Unit

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }: Receive)

  def syncing: Receive = state(HistorySynchronizer.Syncing, {
    case ConsideredValue(Some(networkScore: BigInt), witnesses) =>
      val localScore = history().score()
      if (networkScore > localScore) {
        log.info(s"networkScore=$networkScore > localScore=$localScore")
        val lastIds = history().lastBlockIds(blocksToSend)
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        gotoGettingExtension(networkScore, witnesses)
      } else gotoSynced()
  }: Receive)

  def gettingExtension(betterScore: BigInt, witnesses: Seq[ConnectedPeer]): Receive = state(HistorySynchronizer.GettingExtension, {
    //todo: aggregating function for block ids (like score has) and blockIds type
    case DataFromPeer(msgId, blockIds: Seq[BlockId]@unchecked, connectedPeer)
      if msgId == SignaturesSpec.messageCode &&
        witnesses.contains(connectedPeer) => //todo: ban if non-expected sender

      lastUpdate = System.currentTimeMillis()
      val common = blockIds.head
      log.debug(s"Got blockIds: ${blockIds.map(id => Base58.encode(id))}")

      val toDownload = blockIds.tail.filter(b => !history().contains(b))
      if (history().contains(common) && toDownload.nonEmpty) {
        viewHolder.rollBackTo(common)
        gotoGettingBlocks(witnesses, toDownload.map(_ -> None))
        blockIds.tail.foreach { blockId =>
          val msg = Message(GetBlockSpec, Right(blockId), None)
          val stn = SendToChosen(Seq(connectedPeer))
          networkControllerRef ! NetworkController.SendToNetwork(msg, stn)
        }
      } else {
        log.warn(s"Strange blockIds: $blockIds(${history().contains(common)})")
        gotoSyncing()
      }
  }: Receive)

  def gettingBlocks(witnesses: Seq[ConnectedPeer], blocks: Seq[(BlockId, Option[B])]): Receive =
    state(HistorySynchronizer.GettingBlock, {

      case DataFromPeer(msgId, block: B@unchecked, connectedPeer)
        if msgId == blockMessageSpec.messageCode
          && block.cast[Block[P, TX]].isDefined =>

        lastUpdate = System.currentTimeMillis()
        val blockId = block.id
        log.info("Got block: " + block.encodedId)

        blocks.indexWhere(_._1.sameElements(blockId)) match {
          case i: Int if i == -1 => gotoGettingBlocks(witnesses, blocks)
          case idx: Int =>
            val updBlocks = blocks.updated(idx, blockId -> Some(block))
            if (idx == 0) {
              val toProcess = updBlocks.takeWhile(_._2.isDefined).map(_._2.get)
              log.info(s"Going to process ${toProcess.size} blocks")
              toProcess.find(bp => !processNewBlock(bp, local = false)).foreach { case failedBlock =>
                log.warn(s"Can't apply block: ${failedBlock.json}")
                val hLastId = history().lastBlockIds(1).head
                val fpId = failedBlock.parentId
                if (hLastId sameElements fpId) {
                  connectedPeer.handlerRef ! PeerConnectionHandler.Blacklist
                }
                gotoSyncing()
              }
              if (updBlocks.size > toProcess.size) gotoGettingBlocks(witnesses, updBlocks.drop(toProcess.length))
              else gotoSyncing()
            } else gotoGettingBlocks(witnesses, updBlocks)
        }
    }: Receive)

  //accept only new block from local or remote
  def synced: Receive = state(HistorySynchronizer.Synced, {
    case block: B =>
      processNewBlock(block, local = true)

    case ConsideredValue(Some(networkScore: BigInt), witnesses) =>
      if (networkScore > history().score()) gotoGettingExtension(networkScore, witnesses)

    case DataFromPeer(msgId, block: B@unchecked, _)
      if msgId == blockMessageSpec.messageCode && block.cast[Block[P, TD, CD]].isDefined =>
      processNewBlock(block, local = false)
  }: Receive)

  private def gotoSyncing(): Receive = {
    log.debug("Transition to syncing")
    context become syncing
    scoreObserver ! GetScore
    blockGenerator ! StopGeneration
    syncing
  }

  private def gotoGettingExtension(betterScore: BigInt, witnesses: Seq[ConnectedPeer]): Unit = {
    log.debug("Transition to gettingExtension")
    blockGenerator ! StopGeneration
    context become gettingExtension(betterScore, witnesses)
  }

  private def gotoGettingBlocks(witnesses: Seq[ConnectedPeer], blocks: Seq[(BlockId, Option[B])]): Receive = {
    log.debug("Transition to gettingBlocks")
    context become gettingBlocks(witnesses, blocks)
    gettingBlocks(witnesses, blocks)
  }

  private def gotoSynced(): Receive = {
    log.debug("Transition to synced")
    blockGenerator ! StartGeneration
    context become synced
    synced
  }

  private def processNewBlock(block: B, local: Boolean): Boolean = ???

  /* todo: uncomment/finish
    if (blockValidator.isValid(block, stateHolder)) {
    log.debug(s"New $local block: ${block.json.noSpaces}")
    stateHolder.appendBlock(block, rewardCalculator.changes(block, stateHolder.state)).isSuccess
  } else {
    log.warn("Incorrect new block: " + block.json.noSpaces)
    false
  }
  */
}

object HistorySynchronizer {

  sealed trait Status {
    val name: String
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object GettingExtension extends Status {
    override val name = "getting extension"
  }

  case object GettingBlock extends Status {
    override val name = "getting block"
  }

  case object Synced extends Status {
    override val name = "synced"
  }

  case class CheckBlock(id: ConsensusData.BlockId)

  case object GetStatus

  case object SelfCheck

}*/