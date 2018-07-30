package scorex.core.network

import scorex.core.consensus.ModifierContaining
import scorex.core.network.ModifiersStatus.{Applied, Invalid, ModifiersStatus, Received, Requested, Unknown}
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.{ModifierId, NodeViewModifier}

import scala.collection.concurrent.TrieMap

/**
  * Trait that keeps intermediate modifiers statuses: from Unknown to Applied.
  * It also keeps Invalid modifiers, that should not be downloaded and processed anymore.
  */
trait ModifiersStatusKeeper extends ScorexLogging with ScorexEncoding {

  private val statuses: TrieMap[ModifierId, ModifiersStatus] = TrieMap[ModifierId, ModifiersStatus]()

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId, modifierKeepers: Seq[ModifierContaining[_]]): ModifiersStatus = {
    statuses.getOrElse(id,
      if (modifierKeepers.exists(_.contains(id))) {
        Applied
      } else {
        Unknown
      }
    )
  }

  def status(id: ModifierId, mk: ModifierContaining[_ <: NodeViewModifier]): ModifiersStatus = status(id, Seq(mk))

  def set(id: ModifierId, status: ModifiersStatus): Option[ModifiersStatus] = {
    log.trace(s"Set modifier ${encoder.encode(id)} to status $status")
    if (status == Unknown || status == Applied) {
      statuses.remove(id)
    } else {
      statuses.put(id, status)
    }
  }

  /**
    * Stop tracking this modifier
    */
  def toApplied(id: ModifierId): Option[ModifiersStatus] = set(id, Applied)

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
  def toUnknown(id: ModifierId): Option[ModifiersStatus] = set(id, Unknown)

  /**
    * This modifier is permanently invalid - our not should not try to download and apply it
    */
  def toInvalid(id: ModifierId): Option[ModifiersStatus] = set(id, Invalid)

}
