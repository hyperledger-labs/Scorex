package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import scorex.core.network.ModifiersStatus._
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
  protected val expecting: mutable.Map[ModifierId, ExpectingStatus] = mutable.Map[ModifierId, ExpectingStatus]()

  /**
    * @return size of expecting queue
    */
  def expectingSize: Int = expecting.size

  /**
    * Someone should have these modifiers, but we do not know who
    */
  def onRequest(cp: Option[ConnectedPeer], mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Try[Unit] =
    tryWithLogging(mids.foreach(mid => onRequest(cp, mtid, mid)))

  /**
    * Put modifier id and corresponding peer to expecting map
    * Set modifier to `Requested` state
    */
  protected def onRequest(cp: Option[ConnectedPeer],
                          mtid: ModifierTypeId,
                          mid: ModifierId,
                          checksDone: Int = 0)(implicit ec: ExecutionContext): Unit = {
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(cp, mtid, mid))
    expecting.put(mid, ExpectingStatus(cp, cancellable, checks = checksDone))
    set(mid, Requested)
  }

  /**
    * Modifier was received from remote peer.
    *
    * @return `true` if modifier was expected, `false` otherwise
    */
  def onReceive(mid: ModifierId): Boolean = tryWithLogging {
    if (isExpecting(mid)) {
      stopExpecting(mid)
      set(mid, Received)
      true
    } else {
      set(mid, Unknown)
      false
    }
  }.getOrElse(false)

  /**
    * Modifier was successfully applied to history - set it status to applied
    */
  def onApply(mid: ModifierId): Unit = {
    set(mid, Applied)
  }

  /**
    * Modified is permanently invalid - set it status to invalid
    */
  def onInvalid(mid: ModifierId): Unit = {
    set(mid, Invalid)
  }

  /**
    * We're not trying to process modifier anymore
    * This may happen when received modifier bytes does not correspond to declared modifier id,
    * this modifier was removed from cache because cache is overfull or
    * we stop trying to download this modifiers due to exceeded number of retries
    */
  def stopProcessing(id: ModifierId): Option[ModifiersStatus] = {
    stopExpecting(id)
    set(id, Unknown)
  }

  /**
    *
    * Our node have requested a modifier, but did not received it yet
    * Stops expecting, and expects again if the number of checks does not exceed the maximum
    *
    * @return `true` when expect again, `false` otherwise
    */
  def onStillWaiting(cp: Option[ConnectedPeer],
                     mtid: ModifierTypeId,
                     mid: ModifierId)(implicit ec: ExecutionContext): Try[Unit] = tryWithLogging {
    val checks = expecting(mid).checks + 1
    if (checks < maxDeliveryChecks || cp.isEmpty) {
      stopExpecting(mid)
      onRequest(cp, mtid, mid, checks)
    } else {
      stopProcessing(mid)
      throw new StopExpectingError(mid, checks)
    }
  }

  protected def stopExpecting(mid: ModifierId): Unit = {
    expecting(mid).cancellable.cancel()
    expecting.remove(mid)
  }

  protected[network] def isExpecting(mid: ModifierId): Boolean =
    expecting.contains(mid)

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