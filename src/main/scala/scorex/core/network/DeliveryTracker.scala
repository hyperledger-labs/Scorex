package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.CheckDelivery
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}


/**
  * This class tracks modifier ids which are expected from and delivered by other peers
  * in order to ban or de-prioritize peers which are delivering what is not expected
  */
class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ScorexLogging with ScorexEncoding {

  protected case class ExpectingStatus(peer: Option[ConnectedPeer], cancellable: Cancellable, checks: Int)

  // todo remove
  protected def key(id: ModifierId): ModifierId = id

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  protected val expecting = mutable.Map[ModifierId, ExpectingStatus]()
  protected val delivered = mutable.Map[ModifierId, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[ModifierId, ConnectedPeer]()

  /**
    * Someone should have these modifiers, but we do not know who
    */
  def expect(mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Try[Unit] =
    tryWithLogging(mids.foreach(mid => expect(None, mtid, mid)))

  /**
    * Peer `cp` should have these modifiers
    */
  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Try[Unit] =
    tryWithLogging(mids.foreach(mid => expect(Some(cp), mtid, mid)))

  protected def expect(cp: Option[ConnectedPeer],
                       mtid: ModifierTypeId,
                       mid: ModifierId,
                       checksDone: Int = 0)(implicit ec: ExecutionContext): Unit = {
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(cp, mtid, mid))
    val midAsKey = key(mid)
    expecting.put(midAsKey, ExpectingStatus(cp, cancellable, checks = checksDone))
  }

  /**
    *
    * Stops expecting, and expects again if the number of checks does not exceed the maximum
    *
    * @return `true` when expect again, `false` otherwise
    */
  def reexpect(cp: Option[ConnectedPeer],
               mtid: ModifierTypeId,
               mid: ModifierId)(implicit ec: ExecutionContext): Try[Unit] = tryWithLogging {
    if (isExpecting(mid)) {
      val midAsKey = key(mid)
      val checks = expecting(midAsKey).checks + 1
      stopExpecting(mid)
      if (checks < maxDeliveryChecks) {
        expect(cp, mtid, mid, checks)
      } else {
        throw new StopExpectingError(mid, checks)
      }
    } else {
      expect(cp, mtid, mid)
    }
  }

  protected def stopExpecting(mid: ModifierId): Unit = {
    val midAsKey = key(mid)
    expecting(midAsKey).cancellable.cancel()
    expecting.remove(midAsKey)
  }

  protected[network] def isExpecting(mid: ModifierId): Boolean =
    expecting.contains(key(mid))

  def onReceive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (isExpecting(mid)) {
      stopExpecting(mid)
      delivered(key(mid)) = cp
    } else {
      deliveredSpam(key(mid)) = cp
    }
  }

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  protected def tryWithLogging[T](fn: => T): Try[T] = {
    Try(fn).recoverWith {
      case e: StopExpectingError =>
        log.warn(e.getMessage)
        Failure(e)
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }

  class StopExpectingError(mid: ModifierId, checks: Int)
    extends Error(s"Stop expecting ${encoder.encode(mid)} due to exceeded number of retries $checks")

}