package scorex.core.network

object ModifiersStatus {

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
    * Our node have received this modifier from other peers but did not applied yet.
    * The modifier might be in ModifiersCache or on the way to it
    */
  case object Received extends ModifiersStatus

  /**
    * This modifier was already applied to history
    */
  case object Applied extends ModifiersStatus

  /**
    * This modifier is permanently invalid - never try to download it
    */
  case object Invalid extends ModifiersStatus

}
