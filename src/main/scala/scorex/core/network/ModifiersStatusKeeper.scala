package scorex.core.network

import scorex.core.{ModifierId, NodeViewModifier}
import scorex.core.consensus.{HistoryReader, ModifierContaining}
import scorex.core.network.ModifiersStatus.{Applied, Invalid, ModifiersStatus, Received, Requested, Unknown}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Trait that keeps modifiers statuses when modifier is known, but is not applied yet.
  */
trait ModifiersStatusKeeper {

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

  def set(id: ModifierId, status: ModifiersStatus): Option[ModifiersStatus] = statuses.put(id, status)

  /**
    * Stop tracking this modifier
    */
  def toApplied(id: ModifierId): Option[ModifiersStatus] = statuses.remove(id)

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
  def toUnknown(id: ModifierId): Option[ModifiersStatus] = statuses.remove(id)

  def toInvalid(id: ModifierId): Option[ModifiersStatus] = set(id, Invalid)

}
