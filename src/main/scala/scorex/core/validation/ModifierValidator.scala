package scorex.core.validation


import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexLogging
import scorex.core.validation.ValidationResult._
import scorex.crypto.encode.{Base58, BytesEncoder}

import scala.util.Try

/** Base trait for the modifier validation process.
  *
  * This code was pretty much inspired by cats `Validated` facility. There is a reason for the original cats facility
  * not to suite well for our code. It doesn't suit well for modifier validation in Ergo as being supposed mostly
  * for the web from validation. It's really good in accumulating all the validated fields and constructing
  * a composite object from all these fields.
  *
  * We have a pretty different case, because we need to perform multiple checks for the same object without
  * any transformation. This looks too messy when we try to achieve this via cats `Validated`. See the example of that
  * kind of validation in Ergo `org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor.HeaderValidator`.
  * Some other examples could also be found in `scorex.core.validation.ValidationSpec`.
  *
  * The second distinction from cats `Validated` is that we do support both fail-fast and error-accumulating validation
  * while cats `Validated` supports only accumulative approach.
  */
trait ModifierValidator {

  /** Encoder from bytes to string and back for logging */
  implicit val encoder: BytesEncoder

  /** Start validation in Fail-Fast mode */
  def failFast: ValidationState = ModifierValidator.failFast

  /** Start validation accumulating all the errors */
  def accumulateErrors: ValidationState = ModifierValidator.accumulateErrors

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

  /** unsuccessful validation with a given error*/
  def invalid(error: ModifierError): Invalid = ModifierValidator.invalid(error)

  /** successful validation */
  def success: Valid = ModifierValidator.success

}

object ModifierValidator extends ScorexLogging  {

  /** Start validation in Fail-Fast mode */
  def failFast(implicit e: BytesEncoder): ValidationState = {
    ValidationState(Valid, ValidationStrategy.FailFast)(e)
  }

  /** Start validation accumulating all the errors */
  def accumulateErrors(implicit e: BytesEncoder): ValidationState = {
    ValidationState(Valid, ValidationStrategy.AccumulateErrors)(e)
  }

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = invalid(RecoverableModifierError(errorMessage))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid = invalid(MalformedModifierError(errorMessage))

  /** unsuccessful validation with a given error*/
  def invalid(error: ModifierError): Invalid = Invalid(Seq(error))

  /** successful validation */
  def success: Valid = Valid
}

/** This is the place where all the validation DSL lives */
case class ValidationState(result: ValidationResult, strategy: ValidationStrategy)(implicit e: BytesEncoder) {

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

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values
    */
  def validateEqualIds(given: => Array[Byte], expected: => Array[Byte])(error: String => Invalid): ValidationState = {
    validate(given sameElements expected)(error(s"Given: ${e.encode(given)}, expected ${e.encode(expected)}"))
  }

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState = {
    validateNot(validity == ModifierSemanticValidity.Invalid)(error)
  }

  /** Validate the `condition` is [[scala.util.Success]]. Otherwise the `error` callback will be provided with detail
    * on a failure exception
    */
  def validateTry(condition: => Try[_])(error: Throwable => Invalid): ValidationState = {
    validate(condition.isSuccess)(condition.fold(error, x => error(new Error(x.toString))))
  }

  /** Validate the `block` doesn't throw an Exception. Otherwise the `error` callback will be provided with detail
    * on the exception
    */
  def validateNoThrow(block: => Any)(error: Throwable => Invalid): ValidationState = {
    validateTry(Try(block))(error)
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

  /** Shortcut `require`-like method for the simple validation with fatal error.
    * If you need more convenient checks, use `validate` methods.
    */
  def demand(condition: => Boolean, fatalError: => String): ValidationState = {
    validate(condition)(ModifierValidator.fatal(fatalError))
  }

  /** Shortcut `require`-like method for the [[Try]] validation with fatal error
    */
  def demandSuccess(condition: => Try[_], fatalError: => String): ValidationState = {
    validateTry(condition)(e => ModifierValidator.fatal(fatalError + e.toString))
  }

  /** Shortcut `require`-like method to validate that `block` doesn't throw an Exception.
    * Otherwise returns fatal error
    */
  def demandNoThrow(block: => Any, fatalError: => String): ValidationState = {
    validateNoThrow(block)(e => ModifierValidator.fatal(fatalError + e.toString))
  }

  /** Shortcut `require`-like method for the simple validation with recoverable error.
    * If you need more convenient checks, use `validate` methods.
    */
  def recoverable(condition: => Boolean, recoverableError: => String): ValidationState = {
    validate(condition)(ModifierValidator.error(recoverableError))
  }


  /** Shortcut `require`-like method for the [[Try]] validation with recoverable error
    */
  def recoverableTry(condition: => Try[_], recoverableError: => String): ValidationState = {
    validateTry(condition)(e => ModifierValidator.error(recoverableError + e.toString))
  }

  /** Shortcut `require`-like method to validate that `block` doesn't throw an Exception.
    * Otherwise returns recoverable error
    */
  def recoverableNoThrow(block: => Any, recoverableError: => String): ValidationState = {
    validateNoThrow(block)(e => ModifierValidator.error(recoverableError + e.toString))
  }
}

/** The strategy indicates are we going to perform fail-fast or error-accumulative validation.
  * These two could be also mixed by nested validations.
  */
sealed abstract class ValidationStrategy(val isFailFast: Boolean)

object ValidationStrategy {

  object AccumulateErrors extends ValidationStrategy(false)

  object FailFast extends ValidationStrategy(true)

}
