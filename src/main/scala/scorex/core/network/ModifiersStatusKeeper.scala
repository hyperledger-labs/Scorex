package scorex.core.network

import scorex.core.consensus.ModifierContaining
import scorex.core.network.ModifiersStatus.{Applied, Invalid, ModifiersStatus, Received, Requested, Unknown}
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.{ModifierId, NodeViewModifier}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer

/**
  * Trait that keeps intermediate modifiers statuses: from Unknown to Applied.
  * It also keeps Invalid modifiers, that should not be downloaded and processed anymore.
  */
trait ModifiersStatusKeeper extends ScorexLogging with ScorexEncoding {

  private val invalid: ArrayBuffer[ModifierId] = ArrayBuffer[ModifierId]()

  private val statuses: TrieMap[ModifierId, ModifiersStatus] = TrieMap[ModifierId, ModifiersStatus]()

  /**
    * Number of modifiers in intermediate state - already known, but not applied or marked invalid yet
    */
  def inProcessSize: Int = statuses.size

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId, modifierKeepers: Seq[ModifierContaining[_]]): ModifiersStatus = {
    statuses.getOrElse(id,
      if (invalid.contains(id)) {
        Invalid
      } else if (modifierKeepers.exists(_.contains(id))) {
        Applied
      } else {
        Unknown
      }
    )
  }

  def status(id: ModifierId, mk: ModifierContaining[_ <: NodeViewModifier]): ModifiersStatus = status(id, Seq(mk))

  def status(id: ModifierId): ModifiersStatus = status(id, Seq())

  def set(id: ModifierId, status: ModifiersStatus): Option[ModifiersStatus] = {
    log.trace(s"Set modifier ${encoder.encode(id)} to status $status.")
    if (status == Unknown || status == Applied) {
      statuses.remove(id)
    } else if (status == Invalid) {
      invalid.append(id)
      statuses.remove(id)
    } else {
      statuses.put(id, status)
    }
  }

}
