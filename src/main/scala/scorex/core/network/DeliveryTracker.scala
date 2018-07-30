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
  * This class tracks modifier ids which are expected from other peers
  * in order to ban or de-prioritize peers which are delivering what is not expected
  */
class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ModifiersStatusKeeper with ScorexLogging with ScorexEncoding {

  protected case class ExpectingStatus(peer: Option[ConnectedPeer], cancellable: Cancellable, checks: Int)

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  protected val expecting = mutable.Map[ModifierId, ExpectingStatus]()

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

  /**
    * Put modifier id and corresponding peer to expecting map
    * Set modifier to `Requested` state
    */
  protected def expect(cp: Option[ConnectedPeer],
                       mtid: ModifierTypeId,
                       mid: ModifierId,
                       checksDone: Int = 0)(implicit ec: ExecutionContext): Unit = {
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(cp, mtid, mid))
    expecting.put(mid, ExpectingStatus(cp, cancellable, checks = checksDone))
    toRequested(mid)
  }

  /**
    *
    * Expect modifier, if not expected yet.
    * Stops expecting, and expects again if the number of checks does not exceed the maximum
    *
    * @return `true` when expect again, `false` otherwise
    */
  def reexpect(cp: Option[ConnectedPeer],
               mtid: ModifierTypeId,
               mid: ModifierId)(implicit ec: ExecutionContext): Try[Unit] = tryWithLogging {
    if (isExpecting(mid)) {
      val checks = expecting(mid).checks + 1
      stopExpecting(mid)
      if (checks < maxDeliveryChecks || cp.isEmpty) {
        expect(cp, mtid, mid, checks)
      } else {
        toUnknown(mid)
        throw new StopExpectingError(mid, checks)
      }
    } else {
      expect(cp, mtid, mid)
    }
  }

  protected def stopExpecting(mid: ModifierId): Unit = {
    expecting(mid).cancellable.cancel()
    expecting.remove(mid)
  }

  protected[network] def isExpecting(mid: ModifierId): Boolean =
    expecting.contains(mid)

  /**
    * @return `true` if modifier was expected, `false` otherwise
    */
  def onReceive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean = tryWithLogging {
    if (isExpecting(mid)) {
      stopExpecting(mid)
      toReceived(mid)
      true
    } else {
      false
    }
  }.getOrElse(false)

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