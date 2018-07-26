package scorex.core.network

import scorex.core.ModifierId
import scorex.core.consensus.HistoryReader
import scorex.core.network.ModifiersStatus.{Applied, ModifiersStatus, Received, Requested, Unknown}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Trait that keeps modifiers statuses when modifier is known, but is not applied yet.
  */
trait ModifiersStatusKeeper {

  private val statuses: TrieMap[ModifierIdAsKey, ModifiersStatus] = TrieMap[ModifierIdAsKey, ModifiersStatus]()

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId)(history: HistoryReader[_, _]): ModifiersStatus = {
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
  def toApplied(id: ModifierId): Option[ModifiersStatus] = statuses.remove(key(id))

  /**
    * Modifier `id` was received from other peer
    */
  def toReceived(id: ModifierId): Option[ModifiersStatus] = set(id, Received)

  /**
    * Modifier `id` was requested from other peer
    */
  def toRequested(id: ModifierId): Option[ModifiersStatus] = set(id, Requested)

  /**
    * Remove status for modifier `id` for ModifiersStatusKeeper.
    * This may happen when received modifier bytes does not correspond to declared modifier id
    */
  def toUnknown(id: ModifierId): Option[ModifiersStatus] = statuses.remove(key(id))

  protected type ModifierIdAsKey = mutable.WrappedArray[Byte]

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

}
