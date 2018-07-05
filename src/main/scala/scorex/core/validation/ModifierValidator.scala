package scorex.core.validation


import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexLogging
import scorex.core.validation.ValidationResult._
import scorex.crypto.encode.BytesEncoder

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
  def failFast: ValidationState[Unit] = ModifierValidator.failFast

  /** Start validation accumulating all the errors */
  def accumulateErrors: ValidationState[Unit] = ModifierValidator.accumulateErrors

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

  /** unsuccessful validation with a given error*/
  def invalid(error: ModifierError): Invalid = ModifierValidator.invalid(error)

  /** successful validation */
  def success[T](value: T): Valid[T] = ModifierValidator.success(value)

  def success: Valid[Unit] = ModifierValidator.success

}

object ModifierValidator extends ScorexLogging  {

  /** Start validation in Fail-Fast mode */
  def failFast(implicit e: BytesEncoder): ValidationState[Unit] = {
    ValidationState(ModifierValidator.success, ValidationStrategy.FailFast)(e)
  }

  /** Start validation accumulating all the errors */
  def accumulateErrors(implicit e: BytesEncoder): ValidationState[Unit] = {
    ValidationState(ModifierValidator.success, ValidationStrategy.AccumulateErrors)(e)
  }

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String, cause: Option[Throwable] = None): Invalid =
    invalid(new RecoverableModifierError(errorMessage, cause))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String, cause: Option[Throwable] = None): Invalid =
    invalid(new MalformedModifierError(errorMessage, cause))

  /** unsuccessful validation with a given error*/
  def invalid(error: ModifierError): Invalid = Invalid(Seq(error))

  /** successful validation with payload  */
  def success[T](value: T): Valid[T] = Valid(value)

  /** successful validation without payload */
  val success: Valid[Unit] = success(())
}

/** This is the place where all the validation DSL lives */
case class ValidationState[T](result: ValidationResult[T], strategy: ValidationStrategy)(implicit e: BytesEncoder) {

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(condition: => Boolean)(error: => Invalid): ValidationState[T] = {
    validate(!condition)(error)
  }

  /** Validate the first argument equals the second. This should not be used with `Id` of type `Array[Byte]`.
    * The `error` callback will be provided with detail on argument values for better reporting
    */
  def validateEquals[A](given: => A)(expected: => A)(error: String => Invalid): ValidationState[T] = {
    (given, expected) match {
      case (a: Array[_], b: Array[_]) if a sameElements b =>
        pass(result)
      case (_: Array[_], _) =>
        pass(error(s"Given: $given, expected: $expected. Use validateEqualIds when comparing Arrays"))
      case _ =>
        validate(given == expected)(error(s"Given: $given, expected $expected"))
    }
  }

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values
    */
  def validateEqualIds(given: => Array[Byte], expected: => Array[Byte])
                      (error: String => Invalid): ValidationState[T] = {
    validate(java.util.Arrays.equals(given, expected)) {
      error(s"Given: ${e.encode(given)}, expected ${e.encode(expected)}")
    }
  }

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState[T] = {
    validateNot(validity == ModifierSemanticValidity.Invalid)(error)
  }

  /** Validate the `condition` is `Success`. Otherwise the `error` callback will be provided with detail
    * on a failure exception
    */
  def validateTry(condition: => Try[_])(error: Throwable => Invalid): ValidationState[T] = {
    validate(condition.isSuccess)(condition.fold(error, x => error(new Error(x.toString))))
  }

  /** Validate the `block` doesn't throw an Exception. Otherwise the `error` callback will be provided with detail
    * on the exception
    */
  def validateNoThrow(block: => Any)(error: Throwable => Invalid): ValidationState[T] = {
    validateTry(Try(block))(error)
  }

  /** Validate the condition is `true` or else return the `error` given */
  def validate(condition: => Boolean)(error: => Invalid): ValidationState[T] = {
    pass(if (condition) result else error)
  }

  /** This is for nested validations that allow mixing fail-fast and accumulate-errors validation strategies */
  def validate(operation: => ValidationResult[T]): ValidationState[T] = pass(operation)

  /** Create the next validation state as the result of given `operation` */
  def pass(operation: => ValidationResult[T]): ValidationState[T] = {
    result match {
      case Valid(_) => copy(result = operation)
      case Invalid(_) if strategy.isFailFast => this
      case invalid @ Invalid(_) => copy(result = invalid.accumulateErrors(operation))
    }
  }

  /** Replace payload with the new one, discarding current payload value
    */
  def payload[R](payload: R): ValidationState[R] = {
    copy(result = result(payload))
  }

  /** Map payload if validation is successful
    */
  def map[R](f: T => R): ValidationState[R] = {
    copy(result = result.map(f))
  }

  /** Shortcut `require`-like method for the simple validation with fatal error.
    * If you need more convenient checks, use `validate` methods.
    */
  def demand(condition: => Boolean, fatalError: => String): ValidationState[T] = {
    validate(condition)(ModifierValidator.fatal(fatalError))
  }

  /** Shortcut `require`-like method to validate the `id`s are equal. Otherwise returns fatal error
    */
  def demandEqualIds(given: => Array[Byte], expected: => Array[Byte], fatalError: String): ValidationState[T] = {
    validateEqualIds(given, expected)(d => ModifierValidator.fatal(msg(fatalError, d)))
  }

  /** Shortcut `require`-like method for the `Try` validation with fatal error
    */
  def demandSuccess(condition: => Try[_], fatalError: => String): ValidationState[T] = {
    validateTry(condition)(e => ModifierValidator.fatal(msg(fatalError, e), Option(e)))
  }

  /** Shortcut `require`-like method to validate that `block` doesn't throw an Exception.
    * Otherwise returns fatal error
    */
  def demandNoThrow(block: => Any, fatalError: => String): ValidationState[T] = {
    validateNoThrow(block)(e => ModifierValidator.fatal(msg(fatalError, e), Option(e)))
  }

  /** Shortcut `require`-like method for the simple validation with recoverable error.
    * If you need more convenient checks, use `validate` methods.
    */
  def recoverable(condition: => Boolean, recoverableError: => String): ValidationState[T] = {
    validate(condition)(ModifierValidator.error(recoverableError))
  }


  /** Shortcut `require`-like method to validate the `id`s are equal. Otherwise returns recoverable error
    */
  def recoverableEqualIds(given: => Array[Byte], expected: => Array[Byte],
                          recoverableError: String): ValidationState[T] = {
    validateEqualIds(given, expected)(d => ModifierValidator.error(msg(recoverableError, d)))
  }

  /** Shortcut `require`-like method for the `Try` validation with recoverable error
    */
  def recoverableTry(condition: => Try[_], recoverableError: => String): ValidationState[T] = {
    validateTry(condition)(e => ModifierValidator.error(msg(recoverableError, e), Option(e)))
  }

  /** Shortcut `require`-like method to validate that `block` doesn't throw an Exception.
    * Otherwise returns recoverable error
    */
  def recoverableNoThrow(block: => Any, recoverableError: => String): ValidationState[T] = {
    validateNoThrow(block)(e => ModifierValidator.error(msg(recoverableError, e), Option(e)))
  }

  private def msg(description: String, e: Throwable): String = msg(description, e.getMessage)

  private def msg(description: String, detail: String): String = s"$description: $detail"
}



