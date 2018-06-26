package scorex.core.network

import akka.actor.{ActorContext, ActorRef, Cancellable}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.CheckDelivery
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}


/**
  * This class tracks modifier ids that are expected from and delivered by other peers
  * in order to ban or de-prioritize peers that deliver what is not expected
  */
class DeliveryTracker(context: ActorContext,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ScorexLogging {

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  // todo: Do we need to keep track of ModifierTypeIds? Maybe we could ignore them?

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  protected val expecting = mutable.Set[(ModifierTypeId, ModifierIdAsKey, ConnectedPeer)]()
  protected val delivered = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[ModifierIdAsKey, ConnectedPeer]()

  protected val cancellables = mutable.Map[(ModifierIdAsKey, ConnectedPeer), Cancellable]()
  protected val checksCounter = mutable.Map[(ModifierIdAsKey, ConnectedPeer), Int]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Unit = tryWithLogging {
    for (mid <- mids) expect(cp, mtid, mid)
  }

  protected def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = {
    val cancellable = context.system.scheduler.scheduleOnce(deliveryTimeout,
      nvsRef,
      CheckDelivery(cp, mtid, mid))
    val midAsKey = key(mid)
    expecting += ((mtid, midAsKey, cp))
    cancellables((midAsKey, cp)) = cancellable
  }

  // stops expecting, and expects again if the number of checks does not exceed the maximum
  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = tryWithLogging {
    stopExpecting(cp, mtid, mid)
    val midAsKey = key(mid)
    val checks = checksCounter.getOrElseUpdate((midAsKey, cp), 0) + 1
    checksCounter((midAsKey, cp)) = checks
    if (checks < maxDeliveryChecks) expect(cp, mtid, mid)
    else checksCounter -= ((midAsKey, cp))
  }

  protected def stopExpecting(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId): Unit = {
    val midAsKey = key(mid)
    expecting -= ((mtid, midAsKey, cp))
    cancellables((midAsKey, cp)).cancel()
    cancellables -= ((midAsKey, cp))
  }

  protected def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    expecting.exists(e => (mtid == e._1) && (mid sameElements e._2.array) && cp == e._3)

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (isExpecting(mtid, mid, cp)) {
      val eo = expecting.find(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)
      for (e <- eo) expecting -= e
      delivered(key(mid)) = cp
      val cancellableKey = (key(mid), cp)
      for (c <- cancellables.get(cancellableKey)) c.cancel()
      cancellables -= cancellableKey
    }
    else {
      deliveredSpam(key(mid)) = cp
    }
  }

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  protected def tryWithLogging(fn: => Unit): Unit = {
    Try(fn).recoverWith {
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }

}
