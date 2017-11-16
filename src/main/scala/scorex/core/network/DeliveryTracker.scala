package scorex.core.network

import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable

// This class tracks modifier ids that are expected from and delivered by other peers
// in order to ban or de-prioritize peers that deliver what is not expected
class DeliveryTracker {

  protected type MapKey = scala.collection.mutable.WrappedArray.ofByte
  protected def key(id: ModifierId): MapKey = new mutable.WrappedArray.ofByte(id)

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  protected val expecting = mutable.Set[(ModifierTypeId, ModifierId, ConnectedPeer)]()
  protected val delivered = mutable.Map[MapKey, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[MapKey, ConnectedPeer]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = {
    for (id <- mids) expecting += ((mtid, id, cp))
  }

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    expecting.exists(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = {
    if (isExpecting(mtid, mid, cp)) {
      expecting -= ((mtid, mid, cp))
      delivered(key(mid)) = cp
    }
    else {
      deliveredSpam(key(mid)) = cp
    }
  }

  def delete(mids: Seq[ModifierId]): Unit = for (id <- mids) delivered -= key(id)

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) deliveredSpam -= key(id)

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

}
