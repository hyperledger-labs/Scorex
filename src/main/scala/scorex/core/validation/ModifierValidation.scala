package scorex.core.validation


import scorex.core.consensus.ModifierSemanticValidity
import ValidationResult._
import scorex.crypto.encode.Base58

/** Base trait for the modifier validation process
  */
trait ModifierValidation {

  /** Start validation in Fail-Fast mode */
  def failFast: ValidationState = ValidationState(Valid, ValidationStrategy.FailFast)

  /** Start validation accumulating all the errors */
  def accumulateErrors: ValidationState = ValidationState(Valid, ValidationStrategy.AccumulateErrors)

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = invalid(RecoverableModifierError(errorMessage))

  /** report non-recoverable modifier error that could be fixed by later and requires modifier change */
  def fatal(errorMessage: String): Invalid = invalid(MalformedModifierError(errorMessage))

  /** unsuccessful validation with a given error*/
  def invalid(error: ModifierError): Invalid = Invalid(Seq(error))

  /** successful validation */
  def success: Valid = Valid
}

case class ValidationState(result: ValidationResult, strategy: ValidationStrategy) {

  def validateNot(condition: => Boolean)(error: => Invalid): ValidationState = {
    validate(!condition)(error)
  }

  def validateEquals(given: => Array[Byte], expected: Array[Byte])(error: String => Invalid): ValidationState = {
    validate(given sameElements expected)(error(s"Given: ${Base58.encode(given)}, expected ${Base58.encode(expected)}"))
  }

  def validateEquals[T](given: => T, expected: => T)(error: String => Invalid): ValidationState = {
    validate(given == expected)(error(s"Given: $given, expected $expected"))
  }

  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState = {
    validate(validity == ModifierSemanticValidity.Valid)(error)
  }

  def validate(condition: => Boolean)(error: => Invalid): ValidationState = {
    validate(if (condition) Valid else error)
  }

  def validate(operation: => ValidationResult): ValidationState = {
    result match {
      case Valid => copy(result = operation)
      case Invalid(_) if strategy.isFailFast => this
      case Invalid(_) => copy(result = result ++ operation)
    }
  }
}

sealed abstract class ValidationStrategy(val isFailFast: Boolean)

object ValidationStrategy {

  object AccumulateErrors extends ValidationStrategy(false)

  object FailFast extends ValidationStrategy(true)

}
