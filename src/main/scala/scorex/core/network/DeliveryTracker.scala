package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import scorex.core.consensus.ContainsModifiers
import scorex.core.network.ModifiersStatus._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.CheckDelivery
import scorex.core.network.peer.ConnectedPeer
import scorex.core.utils.ScorexEncoding
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}

/**
  * This class tracks modifier statuses.
  * Modifier can be in one of the following states: Unknown, Requested, Received, Held, Invalid.
  * See ModifiersStatus for states description.
  * Modifiers in `Requested` state are kept in `requested` map containing info about peer and number of retries.
  * Modifiers in `Received` state are kept in `received` set.
  * Modifiers in `Invalid` state are kept in `invalid` set to prevent this modifier download and processing.
  * Modifiers in `Held` state are not kept in this class - we can get this status from object, that contains
  * these modifiers (History for PersistentNodeViewModifier, Mempool for EphemerealNodeViewModifier).
  * If we can't identify modifiers status based on the rules above, it's status is Unknown.
  *
  * In success path modifier changes his statuses `Unknown`->`Requested`->`Received`->`Held`.
  * If something went wrong (e.g. modifier was not delivered) it goes back to `Unknown` state
  * (if we are going to receive it in future) or to `Invalid` state (if we are not going to receive
  * this modifier anymore)
  * Locally generated modifiers may go to `Held` or `Invalid` states at any time.
  * These rules are also described in `isCorrectTransition` function.
  *
  * This class is not thread-save so it should be used only as a local field of an actor
  * and its methods should not be called from lambdas, Future, Future.map, etc.
  */
class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ScorexLogging with ScorexEncoding {

  protected case class RequestedInfo(peer: Option[ConnectedPeer], cancellable: Cancellable, checks: Int)

  // when a remote peer is asked a modifier we add the requested data to `requested`
  protected val requested: mutable.Map[ModifierId, RequestedInfo] = mutable.Map()

  // when our node received invalid modifier we put it to `invalid`
  protected val invalid: mutable.HashSet[ModifierId] = mutable.HashSet()

  // when our node received a modifier we put it to `received`
  protected val received: mutable.Map[ModifierId, ConnectedPeer] = mutable.Map()

  /**
    * @return status of modifier `id`.
    *         Since this class do not keep statuses for modifiers that are already in NodeViewHolder,
    *         `modifierKeepers` are required here to check that modifier is in `Held` status
    */
  def status(id: ModifierId, modifierKeepers: Seq[ContainsModifiers[_]]): ModifiersStatus = {
    if (received.contains(id)) {
      Received
    } else if (requested.contains(id)) {
      Requested
    } else if (invalid.contains(id)) {
      Invalid
    } else if (modifierKeepers.exists(_.contains(id))) {
      Held
    } else {
      Unknown
    }
  }

  def status(id: ModifierId, mk: ContainsModifiers[_ <: NodeViewModifier]): ModifiersStatus = status(id, Seq(mk))

  def status(id: ModifierId): ModifiersStatus = status(id, Seq())

  /**
    *
    * Our node have requested a modifier, but did not received it yet.
    * Stops processing and if the number of checks did not exceed the maximum continue to waiting.
    *
    * @return `true` if number of checks was not exceed, `false` otherwise
    */
  def onStillWaiting(cp: ConnectedPeer,
                     mtid: ModifierTypeId,
                     mid: ModifierId)(implicit ec: ExecutionContext): Try[Unit] = tryWithLogging {
    val checks = requested(mid).checks + 1
    setUnknown(mid)
    if (checks < maxDeliveryChecks) {
      setRequested(mid, mtid,  Some(cp), checks)
    } else {
      throw new StopExpectingError(mid, checks)
    }
  }

  /**
    * Set status of modifier with id `id` to `Requested`
    */
  def setRequested(id: ModifierId,
                   typeId: ModifierTypeId,
                   supplierOpt: Option[ConnectedPeer],
                   checksDone: Int = 0)
                   (implicit ec: ExecutionContext): Unit = tryWithLogging {
    assert(isCorrectTransition(status(id), Requested), s"Illegal status transition: ${status(id)} -> Requested")
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(supplierOpt, typeId, id))
    requested.put(id, RequestedInfo(supplierOpt, cancellable, checksDone))
  }

  def setRequested(ids: Seq[ModifierId], typeId: ModifierTypeId, cp: Option[ConnectedPeer])
                  (implicit ec: ExecutionContext): Unit = ids.foreach(setRequested(_, typeId, cp))

  /**
    * Modified with id `id` is permanently invalid - set its status to `Invalid`
    * and return [[ConnectedPeer]] which sent bad modifier.
    */
  def setInvalid(id: ModifierId): Option[ConnectedPeer] = {
    val oldStatus: ModifiersStatus = status(id)
    assert(isCorrectTransition(oldStatus, Invalid), s"Illegal status transition: $oldStatus -> Invalid")
    val senderOpt = oldStatus match {
      case Requested =>
        requested(id).cancellable.cancel()
        requested.remove(id).flatMap(_.peer)
      case Received =>
        received.remove(id)
      case _ =>
        None
    }
    invalid.add(id)
    senderOpt
  }

  /**
    * Modifier with id `id` was successfully applied to history - set its status to `Held`.
    */
  def setHeld(id: ModifierId): Unit = {
    val oldStatus: ModifiersStatus = status(id)
    assert(isCorrectTransition(oldStatus, Held), s"Illegal status transition: $oldStatus -> Held")
    clearStatusForModifier(id, oldStatus) // we need only to clear old status in this case
  }

  /**
    * Set status of modifier with id `id` to `Unknown`.
    *
    * We're not trying to process modifier anymore in this case.
    * This may happen when received modifier bytes does not correspond to declared modifier id,
    * this modifier was removed from cache because cache is overfull or
    * we stop trying to download this modifiers due to exceeded number of retries
    */
  def setUnknown(id: ModifierId): Unit = {
    val oldStatus: ModifiersStatus = status(id)
    assert(isCorrectTransition(oldStatus, Unknown), s"Illegal status transition: $oldStatus -> Unknown")
    clearStatusForModifier(id, oldStatus) // we need only to clear old status in this case
  }

  /**
    * Modifier with id `id`  was received from remote peer - set its status to `Received`.
    */
  def setReceived(id: ModifierId, sender: ConnectedPeer): Unit = {
    val oldStatus: ModifiersStatus = status(id)
    assert(isCorrectTransition(oldStatus, Invalid), s"Illegal status transition: $oldStatus -> Received")
    if (oldStatus != Received) {
      requested(id).cancellable.cancel()
      requested.remove(id)
      received.put(id, sender)
    }
  }

  /**
    * Self-check that transition between states is correct.
    *
    * Modifier may stay in current state,
    * go to Requested state form Unknown
    * go to Received state from Requested
    * go to Invalid state from any state (this may happen on invalid locally generated modifier)
    * go to Unknown state from Requested and Received states
    */
  private def isCorrectTransition(oldStatus: ModifiersStatus, newStatus: ModifiersStatus): Boolean = {
    oldStatus match {
      case old if old == newStatus => true
      case _ if newStatus == Invalid || newStatus == Held => true
      case Unknown => newStatus == Requested
      case Requested => newStatus == Unknown || newStatus == Received
      case Received => newStatus == Unknown
      case _ => false
    }
  }

  private def tryWithLogging[T](fn: => T): Try[T] = {
    Try(fn).recoverWith {
      case e: StopExpectingError =>
        log.warn(e.getMessage)
        Failure(e)
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }

  private def clearStatusForModifier(id: ModifierId, oldStatus: ModifiersStatus): Unit = {
    oldStatus match {
      case Requested =>
        requested(id).cancellable.cancel()
        requested.remove(id).flatMap(_.peer)
      case Received =>
        received.remove(id)
      case _ =>
        ()
    }
  }

  class StopExpectingError(mid: ModifierId, checks: Int)
    extends Error(s"Stop expecting ${encoder.encodeId(mid)} due to exceeded number of retries $checks")

}
