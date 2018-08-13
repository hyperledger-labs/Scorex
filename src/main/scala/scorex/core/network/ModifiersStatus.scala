package scorex.core.network

sealed trait ModifiersStatus

object ModifiersStatus {

  /**
    * This modifier is unknown to our node
    */
  case object Unknown extends ModifiersStatus

  /**
    * Our node have requested this modifier from other peers but did not received it yet.
    */
  case object Requested extends ModifiersStatus

  /**
    * Our node have received this modifier from other peers but did not applied yet.
    * The modifier might be in ModifiersCache or on the way to it
    */
  case object Received extends ModifiersStatus

  /**
    * This modifier is already on NodeViewHoder - applied to History if it is PersistentModifier or
    * in MemPool if it is Ephemereal modifier.
    */
  case object Held extends ModifiersStatus

  /**
    * This modifier is permanently invalid - never try to download it
    */
  case object Invalid extends ModifiersStatus

}
