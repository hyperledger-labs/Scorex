package scorex.core.validation

import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoder
import scorex.core.validation.ValidationResult.{Invalid, Valid}
import scorex.util.ModifierId

import scala.util.Try

/** This is the place where all the validation DSL lives */
final case class ValidationState[T](result: ValidationResult[T], strategy: ValidationStrategy)
                                   (implicit e: ScorexEncoder) {

  /** Create the next validation state as the result of given `operation` */
  def pass[R](operation: => ValidationResult[R]): ValidationState[R] =
    result match {
      case Valid(_) => copy(result = operation)
      case Invalid(_) if strategy.isFailFast || result == operation => asInstanceOf[ValidationState[R]]
      case invalid@Invalid(_) => copy(result = invalid.accumulateErrors(operation))
    }

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(condition: => Boolean)(error: => Invalid): ValidationState[T] =
    validate(!condition)(error)

  /** Validate the first argument equals the second. This should not be used with `ModifierId` of type `Array[Byte]`.
    * The `error` callback will be provided with detail on argument values for better reporting
    */
  def validateEquals[A](given: => A, expected: => A)(error: String => Invalid): ValidationState[T] =
    pass((given, expected) match {
      case (a: Array[_], b: Array[_]) if a sameElements b => result
      case (_: Array[_], _) => error(s"Given: $given, expected: $expected. Use validateEqualIds when comparing Arrays")
      case _ if given == expected => result
      case _ => error(s"Given: $given, expected $expected")
    })

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values
    */
  def validateEqualIds(given: => ModifierId, expected: => ModifierId)
                      (error: String => Invalid): ValidationState[T] =
    validate(given == expected) {
      error(s"Given: ${e.encodeId(given)}, expected ${e.encodeId(expected)}")
    }

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState[T] =
    validateNot(validity == ModifierSemanticValidity.Invalid)(error)

  /** Validate the condition is `true` or else return the `error` given
    */
  def validate(condition: => Boolean)(error: => Invalid): ValidationState[T] =
    pass(if (condition) result else error)

  /** Replace payload with the new one, discarding current payload value. This method catches throwables
    */
  def payload[R](payload: => R): ValidationState[R] =
    pass(result(payload))

  /** Map payload if validation is successful
    */
  def payloadMap[R](f: T => R): ValidationState[R] =
    copy(result = result.map(f))

  /** Validate the `condition` is `Success`. Otherwise the `error` callback will be provided with detail
    * on a failure exception
    */
  def validateNoFailure(condition: => Try[_])(error: Throwable => Invalid): ValidationState[T] =
    pass(condition.fold(error, _ => result))

  /** Validate the `block` doesn't throw an Exception. Otherwise the `error` callback will be provided with detail
    * on the exception
    */
  def validateNoThrow(block: => Any)(error: Throwable => Invalid): ValidationState[T] =
    validateNoFailure(Try(block))(error)

  /** Validate `condition` against payload is `true` or else return the `error`
    */
  def validateTry[A](tryValue: => Try[A], error: Throwable => Invalid)
                    (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] =
    pass(tryValue.fold(error, v => operation(this, v)))

  /** Validate condition against option value if it's not `None`.
    * If given option is `None` then pass the previous result as success.
    * Return `error` if option is `Some` amd condition is `false`
    */
  def validateOrSkip[A](option: => Option[A])
                       (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] =
    option
      .map(value => pass(operation(this, value)))
      .getOrElse(this)

  /** This could add some sugar when validating elements of a given collection
    */
  def validateSeq[A](seq: Iterable[A])
                    (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] =
    seq.foldLeft(this) { (state, elem) =>
      state.pass(operation(state, elem))
    }

  /** This is for nested validations that allow mixing fail-fast and accumulate-errors validation strategies
    */
  def validate(operation: => ValidationResult[T]): ValidationState[T] = pass(operation)

  /** Shortcut `require`-like method for the simple validation with fatal error.
    * If you need more convenient checks, use `validate` methods.
    */
  def demand(condition: => Boolean, fatalError: => String): ValidationState[T] =
    validate(condition)(ModifierValidator.fatal(fatalError))

  /** Shortcut `require`-like method to validate the `id`s are equal. Otherwise returns fatal error
    */
  def demandEqualIds(given: => ModifierId, expected: => ModifierId, fatalError: String): ValidationState[T] =
    validateEqualIds(given, expected)(d => ModifierValidator.fatal(fatalError, d))

  /** Shortcut `require`-like method to validate the arrays are equal. Otherwise returns fatal error
    */
  def demandEqualArrays(given: => Array[Byte], expected: => Array[Byte], fatalError: String): ValidationState[T] =
    validate(java.util.Arrays.equals(given, expected)) {
      ModifierValidator.fatal(s"$fatalError. Given ${e.encode(given)} while expected ${e.encode(expected)}")
    }

  /** Shortcut `require`-like method for the `Try` validation with fatal error
    */
  def demandSuccess(condition: => Try[_], fatalError: => String): ValidationState[T] =
    validateNoFailure(condition)(e => ModifierValidator.fatal(fatalError, e))

  /** Shortcut `require`-like method to validate that `block` doesn't throw an Exception.
    * Otherwise returns fatal error
    */
  def demandNoThrow(block: => Any, fatalError: => String): ValidationState[T] =
    validateNoThrow(block)(e => ModifierValidator.fatal(fatalError, e))

  def demandTry[A](tryValue: => Try[A], fatalError: => String)
                  (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] =
    validateTry(tryValue, e => ModifierValidator.fatal(fatalError, e))(operation)

  /** Shortcut `require`-like method for the simple validation with recoverable error.
    * If you need more convenient checks, use `validate` methods.
    */
  def recoverable(condition: => Boolean, recoverableError: => String): ValidationState[T] =
    validate(condition)(ModifierValidator.error(recoverableError))

  /** Shortcut `require`-like method to validate the `id`s are equal. Otherwise returns recoverable error
    */
  def recoverableEqualIds(given: => ModifierId, expected: => ModifierId,
                          recoverableError: String): ValidationState[T] =
    validateEqualIds(given, expected)(d => ModifierValidator.error(recoverableError, d))

}
