package scorex.core.validation

import scorex.core.validation.ValidationResult.Invalid

/**
  * Configuration of validation.
  *
  * Specifies the strategy to by used (fail-fast or error-accumulative) set of
  * activated validation rules and their error messages
  */
abstract class ValidationSettings {
  val isFailFast: Boolean

  def getMessage(id: Short): Invalid

  def isActive(id: Short): Boolean
}

class MapValidationSettings(override val isFailFast: Boolean,
                                map: Map[Short, (Invalid, Boolean)]) extends ValidationSettings {

  override def getMessage(id: Short): Invalid = {
    map.get(id).map(_._1).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = map.get(id).exists(_._2)

}
