package scorex.core.network

import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable

// This class tracks modifier ids that are expected from and delivered by other peers
// in order to ban or de-prioritize peers that deliver what is not expected
class DeliveryTracker {

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  protected val expecting = mutable.Set[(ModifierTypeId, ModifierId, ConnectedPeer)]()
  protected val delivered = mutable.Map[ModifierId, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[ModifierId, ConnectedPeer]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = {
    for (id <- mids) expecting += ((mtid, id, cp))
  }

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    expecting.exists(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    if (isExpecting(mtid, mid, cp)) {
      expecting -= ((mtid, mid, cp))
      delivered(mid) = cp
    }
    else {
      deliveredSpam(mid) = cp
    }
  }

  def delete(mids: Seq[ModifierId]): Unit = for (id <- mids) delivered -= id

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) deliveredSpam -= id

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains mid

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(mid)

}
