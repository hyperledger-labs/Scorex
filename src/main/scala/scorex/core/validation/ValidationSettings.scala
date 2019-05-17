package scorex.core.validation

import scorex.core.validation.ValidationResult.Invalid

/**
  * Specifies the strategy to by used (fail-fast or error-accumulative), a set of
  * activated validation rules with corresponding error messages
  */
abstract class ValidationSettings {
  val isFailFast: Boolean

  def getError(id: Short, e: Throwable): Invalid = getError(id, e.getMessage)

  def getError(id: Short, details: String): Invalid

  def getError(id: Short): Invalid = getError(id, "")

  def isActive(id: Short): Boolean
}
