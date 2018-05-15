package scorex.core.validation


import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.validation.ValidationResult._
import scorex.crypto.encode.Base58

/** Base trait for the modifier validation process
  */
trait ModifierValidator {

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

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(condition: => Boolean)(error: => Invalid): ValidationState = {
    validate(!condition)(error)
  }

  /** Validate the first argument equals the second. This should not be used with `Id` of type `Array[Byte]`.
    * The `error` callback will be provided with detail on argument values for better reporting
    */
  def validateEquals[T](given: => T)(expected: => T)(error: String => Invalid): ValidationState = {
    (given, expected) match {
      case (a: Array[_], b: Array[_]) if a sameElements b =>
        pass(Valid)
      case (_: Array[_], _) =>
        pass(error(s"Given: $given, expected: $expected. Use validateEqualIds when comparing Arrays"))
      case _ =>
        validate(given == expected)(error(s"Given: $given, expected $expected"))
    }
  }

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values */
  def validateEqualIds(given: => Array[Byte], expected: => Array[Byte])(error: String => Invalid): ValidationState = {
    validate(given sameElements expected)(error(s"Given: ${Base58.encode(given)}, expected ${Base58.encode(expected)}"))
  }


  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState = {
    validateNot(validity == ModifierSemanticValidity.Invalid)(error)
  }

  /** Validate the condition is `true` or else return the `error` given */
  def validate(condition: => Boolean)(error: => Invalid): ValidationState = {
    pass(if (condition) Valid else error)
  }

  /** This is for nested validations that allow mixing fail-fast and accumulate-errors validation strategies */
  def validate(operation: => ValidationResult): ValidationState = pass(operation)

  /** Create the next validation state as the result of given `operation` */
  def pass(operation: => ValidationResult): ValidationState = {
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
