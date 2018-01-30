package scorex.core.network

import akka.actor.{ActorContext, ActorRef, Cancellable}
import scorex.core.LocalInterface
import scorex.core.consensus.History
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.network.NodeViewSynchronizer.SendLocalSyncInfo
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * SyncTracker caches the peers' statuses (i.e. whether they are ahead or behind this node)
  */
class SyncTracker(nvsRef: ActorRef,
                  context: ActorContext,
                  networkSettings: NetworkSettings,
                  localInterfaceRef: ActorRef,
                  timeProvider: NetworkTimeProvider) extends ScorexLogging {

  import History.HistoryComparisonResult._
  import NodeViewSynchronizer.Timestamp

  private var schedule: Option[Cancellable] = None

  private val status = mutable.Map[ConnectedPeer, HistoryComparisonResult.Value]()
  private val lastSyncSentTime = mutable.Map[ConnectedPeer, Timestamp]()
  private val lastSyncReceivedTime = mutable.Map[ConnectedPeer, Timestamp]()

  private var lastSyncInfoSentTime: Timestamp = 0L

  private var stableSyncRegime = false

  def scheduleSendSyncInfo(): Unit = {
    if (schedule.isDefined) schedule.get.cancel()
    schedule = Some(context.system.scheduler.schedule(2.seconds, minInterval())(nvsRef ! SendLocalSyncInfo))
  }

  def maxInterval(): FiniteDuration = if (stableSyncRegime) networkSettings.syncStatusRefreshStable else networkSettings.syncStatusRefresh

  def minInterval(): FiniteDuration = if (stableSyncRegime) networkSettings.syncIntervalStable else networkSettings.syncInterval

  def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult.Value): Unit = {
    val seniorsBefore = numOfSeniors()
    this.status(peer) = status
    val seniorsAfter = numOfSeniors()

    if (seniorsBefore > 0 && seniorsAfter == 0) {
      log.info("Syncing is done, switching to stable regime")
      stableSyncRegime = true
      scheduleSendSyncInfo()
      localInterfaceRef ! LocalInterface.NoBetterNeighbour
    }
    if (seniorsBefore == 0 && seniorsAfter > 0) {
      localInterfaceRef ! LocalInterface.BetterNeighbourAppeared
    }
  }

  def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime = timeProvider.time()
    lastSyncSentTime(peer) = currentTime
    lastSyncInfoSentTime = currentTime
  }

  def elapsedTimeSinceLastSync(): Long = {
    timeProvider.time() - lastSyncInfoSentTime
  }

  def updateLastSyncReceivedTime(peer: ConnectedPeer): Unit = {
    lastSyncReceivedTime(peer) = timeProvider.time()
  }

  private def outdatedPeers(): Seq[ConnectedPeer] = lastSyncSentTime
    .filter(t => (System.currentTimeMillis() - t._2).millis > maxInterval()).keys.toSeq

  private def numOfSeniors(): Int = status.count(_._2 == Older)

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    */
  def peersToSyncWith(): Seq[ConnectedPeer] = {
    val outdated = outdatedPeers()

    lazy val unknowns = status.filter(_._2 == HistoryComparisonResult.Unknown).keys.toIndexedSeq
    lazy val olders = status.filter(_._2 == HistoryComparisonResult.Older).keys.toIndexedSeq
    lazy val nonOutdated = if (olders.nonEmpty) olders(scala.util.Random.nextInt(olders.size)) +: unknowns else unknowns

    val peers = if (outdated.nonEmpty) outdated
      else nonOutdated.filter(p => (timeProvider.time() - lastSyncSentTime.getOrElse(p, 0L)).millis >= minInterval)

    peers.foreach(updateLastSyncSentTime)
    peers
  }
}
