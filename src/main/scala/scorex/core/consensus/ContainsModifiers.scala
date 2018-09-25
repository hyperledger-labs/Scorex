package scorex.core.consensus

import scorex.core.NodeViewModifier
import scorex.util.ModifierId

/**
  * Object that contains modifiers of type `MOD`
  */
trait ContainsModifiers[MOD <: NodeViewModifier] {

  /**
    *
    * @param persistentModifier -  modifier to check
    * @return `true` if this object contains this modifier, `false` otherwise
    */
  def contains(persistentModifier: MOD): Boolean = contains(persistentModifier.id)

  /**
    *
    * @param id -  modifier's id
    * @return `true` if this object contains modifier with specified id, `false` otherwise
    */
  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  /**
    * @param modifierId - modifier id to get
    * @return modifier of type MOD with id == modifierId if exist
    */
  def modifierById(modifierId: ModifierId): Option[MOD]
}
