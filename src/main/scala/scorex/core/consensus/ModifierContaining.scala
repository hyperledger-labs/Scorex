package scorex.core.consensus

import scorex.core.{ModifierId, NodeViewModifier}

trait ModifierContaining[MOD <: NodeViewModifier] {


  /**
    * Whether the history contains the given modifier
    *
    * @param persistentModifier - modifier
    * @return
    */
  def contains(persistentModifier: MOD): Boolean = contains(persistentModifier.id)

  /**
    *
    * @param id -  modifier's id
    * @return `true` if this object contains modifier with specified id, `false` otherwise
    */
  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  /**
    * @param modifierId - modifier id to get from this class
    * @return modifier of type MOD with id == modifierId if exist
    */
  def modifierById(modifierId: ModifierId): Option[MOD]
}
