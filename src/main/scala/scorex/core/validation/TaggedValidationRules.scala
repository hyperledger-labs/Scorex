package scorex.core.validation

import scorex.core.validation.ValidationResult.Invalid

/**
  * Validation rules defining how to treat particular tagged rule.
  */
abstract class TaggedValidationRules {

  def getError(id: Short, e: Throwable): Invalid = getError(id, e.getMessage)

  def getError(id: Short, details: String): Invalid

  def getError(id: Short): Invalid = getError(id, "")

  def isActive(id: Short): Boolean
}
