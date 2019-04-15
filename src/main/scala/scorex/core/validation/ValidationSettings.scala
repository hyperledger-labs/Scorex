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

  def getError(id: Short, e: Throwable): Invalid = getError(id, e.getMessage)

  def getError(id: Short, details: String): Invalid

  def getError(id: Short): Invalid = getError(id, "")

  def isActive(id: Short): Boolean
}

class MapValidationSettings(override val isFailFast: Boolean,
                            map: Map[Short, (String => Invalid, Boolean)]) extends ValidationSettings {


  override def getError(id: Short, details: String): Invalid = {
    map.get(id).map(_._1(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = map.get(id).forall(_._2)

}
