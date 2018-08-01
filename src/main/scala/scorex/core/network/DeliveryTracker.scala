package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import scorex.core.consensus.ModifierContaining
import scorex.core.network.ModifiersStatus._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.CheckDelivery
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}


/**
  * This class tracks modifier statuses: from Unknown to Applied.
  * It also keeps Invalid modifiers, that should not be downloaded and processed anymore.
  */
class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ScorexLogging with ScorexEncoding {

  protected case class ExpectingStatus(peer: Option[ConnectedPeer], cancellable: Cancellable, checks: Int)

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  protected val expecting: mutable.Map[ModifierId, ExpectingStatus] = mutable.Map[ModifierId, ExpectingStatus]()

  protected val invalid: ArrayBuffer[ModifierId] = ArrayBuffer[ModifierId]()

  protected val statuses: TrieMap[ModifierId, ModifiersStatus] = TrieMap[ModifierId, ModifiersStatus]()

  /**
    * @return size of expecting queue
    */
  def expectingSize: Int = expecting.size

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId, modifierKeepers: Seq[ModifierContaining[_]]): ModifiersStatus = {
    statuses.getOrElse(id,
      if (invalid.contains(id)) {
        Invalid
      } else if (expecting.contains(id)) {
        Requested
      } else if (modifierKeepers.exists(_.contains(id))) {
        Applied
      } else {
        Unknown
      }
    )
  }

  def status(id: ModifierId, mk: ModifierContaining[_ <: NodeViewModifier]): ModifiersStatus = status(id, Seq(mk))

  def status(id: ModifierId): ModifiersStatus = status(id, Seq())

  /**
    * Someone should have these modifiers, but we do not know who
    */
  def onRequest(cp: Option[ConnectedPeer], mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Try[Unit] =
    tryWithLogging(mids.foreach(mid => onRequest(cp, mtid, mid)))

  /**
    * Put modifier id and corresponding peer to expecting map
    */
  protected def onRequest(cp: Option[ConnectedPeer],
                          mtid: ModifierTypeId,
                          mid: ModifierId,
                          checksDone: Int = 0)(implicit ec: ExecutionContext): Unit = {
    updateStatus(mid, Requested)
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(cp, mtid, mid))
    expecting.put(mid, ExpectingStatus(cp, cancellable, checks = checksDone))
  }

  /**
    * Modifier was received from remote peer.
    *
    * @return `true` if modifier was expected, `false` otherwise
    */
  def onReceive(mid: ModifierId): Boolean = tryWithLogging {
    if (isExpecting(mid)) {
      updateStatus(mid, Received)
      true
    } else {
      updateStatus(mid, Unknown)
      false
    }
  }.getOrElse(false)

  /**
    * Modifier was successfully applied to history - set it status to applied
    */
  def onApply(mid: ModifierId): Unit = {
    updateStatus(mid, Applied)
  }

  /**
    * Modified is permanently invalid - set it status to invalid
    */
  def onInvalid(mid: ModifierId): Unit = {
    updateStatus(mid, Invalid)
  }

  /**
    * We're not trying to process modifier anymore
    * This may happen when received modifier bytes does not correspond to declared modifier id,
    * this modifier was removed from cache because cache is overfull or
    * we stop trying to download this modifiers due to exceeded number of retries
    */
  def stopProcessing(id: ModifierId): Unit = {
    stopExpecting(id)
    updateStatus(id, Unknown)
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

  /**
    * Set status of modifier with id `id` to `newStatus`
    */
  protected def updateStatus(id: ModifierId, newStatus: ModifiersStatus): ModifiersStatus = {
    val oldStatus: ModifiersStatus = status(id)
    log.debug(s"Set modifier ${encoder.encode(id)} from status $oldStatus to status $newStatus.")
    if (oldStatus == Requested) {
      stopExpecting(id)
    }
    if (newStatus == Unknown || newStatus == Applied || newStatus == Requested) {
      // no need to keep this status as soon as it is already kept in different storage
      statuses.remove(id)
    } else if (newStatus == Invalid) {
      invalid.append(id)
      statuses.remove(id)
    } else {
      statuses.put(id, newStatus)
    }
    oldStatus
  }.ensuring(oldStatus => isCorrectTransition(oldStatus, newStatus))

  /**
    * Self-check, that transition between states is correct
    */
  protected def isCorrectTransition(oldStatus: ModifiersStatus, newStatus: ModifiersStatus): Boolean = {
    oldStatus match {
      case old if old == newStatus => true
      case old if newStatus == Applied => true
      case Unknown => newStatus == Requested
      case Requested => newStatus == Received || newStatus == Unknown
      case Received => newStatus == Applied || newStatus == Invalid || newStatus == Unknown
      case _ => false
    }
  }

  class StopExpectingError(mid: ModifierId, checks: Int)
    extends Error(s"Stop expecting ${encoder.encode(mid)} due to exceeded number of retries $checks")

}