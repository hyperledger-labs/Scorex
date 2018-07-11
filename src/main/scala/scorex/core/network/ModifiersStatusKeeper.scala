package scorex.core.network

import scorex.core.ModifierId
import scorex.core.consensus.History
import scorex.core.network.ModifiersStatusKeeper._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Class that keeps modifiers statuses when modifier is known, but is not applied yet.
  */
class ModifiersStatusKeeper() {

  type K = mutable.WrappedArray[Byte]

  // TODO some LRU cache might be useful here to limit this size and remove outdated statuses
  private val statuses: TrieMap[K, ModifiersStatus] = TrieMap[K, ModifiersStatus]()

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId)(history: History[_, _, _]): ModifiersStatus = {
    val mKey = key(id)
    statuses.getOrElse(mKey,
      if (history.contains(id)) {
        Applied
      } else {
        Unknown
      }
    )
  }

  def set(id: ModifierId, status: ModifiersStatus): Option[ModifiersStatus] = statuses.put(key(id), status)

  /**
    * We do not keep applied modifiers status since we can get their status from history
    */
  def applied(id: ModifierId): Option[ModifiersStatus] = statuses.remove(key(id))

  /**
    * Modifier `id` was received from other peer
    */
  def received(id: ModifierId): Option[ModifiersStatus] = set(id, Received)

  /**
    * Modifier `id` was requested from other peer
    */
  def requested(id: ModifierId): Option[ModifiersStatus] = set(id, Requested)

  /**
    * Modifier `id` was received with incorrect bytes (unable to parse or
    * calculated id is different from the declared one)
    */
  def incorrectBytes(id: ModifierId): Option[ModifiersStatus] = statuses.remove(key(id))

  protected def key(id: ModifierId) = new mutable.WrappedArray.ofByte(id)
}


object ModifiersStatusKeeper {

  sealed trait ModifiersStatus

  /**
    * This modifier is unknown to our node
    */
  case object Unknown extends ModifiersStatus

  /**
    * Our node have requested this modifier from other peers but did not received it yet.
    */
  case object Requested extends ModifiersStatus

  /**
    * Our node have received this modifier from other peers but is not applied yet.
    * It might be ModifiersCache or on the way to it
    */
  case object Received extends ModifiersStatus

  /**
    * This modifier was already applied to history
    */
  case object Applied extends ModifiersStatus

}
