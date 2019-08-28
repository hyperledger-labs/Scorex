package scorex.core.validation

import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoder
import scorex.core.validation.ValidationResult.{Invalid, Valid}
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

/** Allows to disable validation for particular rule (set of rules) by id.
  * This is the place where all the validation DSL lives */
final case class TaggedValidationState[T](result: ValidationResult[T],
                                          strategy: ValidationStrategy,
                                          settings: TaggedValidationRules)
                                         (implicit e: ScorexEncoder) {

  /** Create the next validation state as the result of given `operation` */
  def pass[R](operation: => ValidationResult[R]): TaggedValidationState[R] =
    result match {
      case Valid(_) => copy(result = operation)
      case Invalid(_) if strategy.isFailFast || result == operation => asInstanceOf[TaggedValidationState[R]]
      case invalid@Invalid(_) => copy(result = invalid.accumulateErrors(operation))
    }

  /** Replace payload with the new one, discarding current payload value. This method catches throwables
    */
  def payload[R](payload: => R): TaggedValidationState[R] =
    pass(result(payload))

  /** Map payload if validation is successful
    */
  def payloadMap[R](f: T => R): TaggedValidationState[R] =
    copy(result = result.map(f))

  /** Validate the condition is `true` or else return the `error` given
    */
  def validate(id: Short, condition: => Boolean, details: String = ""): TaggedValidationState[T] =
    pass(if (!settings.isActive(id) || condition) result else settings.getError(id, details))

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(id: Short, condition: => Boolean, details: String = ""): TaggedValidationState[T] =
    validate(id, !condition, details)

  /** Validate the first argument equals the second. This should not be used with `ModifierId` of type `Array[Byte]`.
    * The `error` callback will be provided with detail on argument values for better reporting
    */
  def validateEquals[A](id: Short, given: => A, expected: => A): TaggedValidationState[T] =
    pass((given, expected) match {
      case _ if !settings.isActive(id) => result
      case (a: Array[_], b: Array[_]) if a sameElements b => result
      case (_: Array[_], _) => settings.getError(id, s"Given: $given, expected: $expected. Use validateEqualIds when comparing Arrays")
      case _ if given == expected => result
      case _ => settings.getError(id, s"Given: $given, expected $expected")
    })

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values
    */
  def validateEqualIds(id: Short, given: => ModifierId, expected: => ModifierId): TaggedValidationState[T] =
    pass {
      if (!settings.isActive(id) || given == expected) result
      else settings.getError(id, s"Given: ${e.encodeId(given)}, expected ${e.encodeId(expected)}")
    }

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(id: Short, validity: => ModifierSemanticValidity, details: String = ""
                       ): TaggedValidationState[T] =
    validateNot(id, validity == ModifierSemanticValidity.Invalid, details)

  /** Validate the `condition` is `Success`. Otherwise the `error` callback will be provided with detail
    * on a failure exception
    */
  def validateNoFailure(id: Short, condition: => Try[_]): TaggedValidationState[T] =
    pass(if (!settings.isActive(id)) result else condition.fold(e => settings.getError(id, e), _ => result))

  /** Validate the `block` doesn't throw an Exception. Otherwise the `error` callback will be provided with detail
    * on the exception
    */
  def validateNoThrow(id: Short, block: => Any): TaggedValidationState[T] =
    validateNoFailure(id, Try(block))

  /** Validate `operation` against payload is `Valid` or else return the `error`
    */
  def validateTry[A](tryValue: => Try[A], error: Throwable => Invalid)
                    (operation: (TaggedValidationState[T], A) => ValidationResult[T]): TaggedValidationState[T] =
    pass(tryValue.fold(error, v => operation(this, v)))

  /** Validate `condition` against payload is `true` or else return the `error`
    */
  def validateTryFlatten(id: Short, operation: T => Try[T], condition: T => Boolean): TaggedValidationState[T] =
    pass(result.toTry.flatMap(r => operation(r)) match {
      case Failure(ex) => settings.getError(id, ex)
      case Success(v) if settings.isActive(id) && !condition(v) => settings.getError(id)
      case Success(v) => result(v)
    })

  /** Validate `operation` against option value if it's not `None`.
    * If given option is `None` then pass the previous result as success.
    * Return `error` if option is `Some` amd condition is `Invalid`
    */
  def validateOrSkip[A](option: => Option[A])
                       (operation: (TaggedValidationState[T], A) => ValidationResult[T]): TaggedValidationState[T] =
    option
      .map(value => pass(operation(this, value)))
      .getOrElse(this)

  /** Validate condition against option value if it's not `None`.
    * If given option is `None` then pass the previous result as success.
    * Return `error` if option is `Some` amd condition is `false`
    */
  def validateOrSkipFlatten[A](id: Short, option: => Option[A], condition: A => Boolean): TaggedValidationState[T] =
    pass(option match {
      case Some(v) if settings.isActive(id) && !condition(v) => settings.getError(id)
      case _ => result
    })

  /** This could add some sugar when validating elements of a given collection
    */
  def validateSeq[A](seq: Iterable[A])
                    (operation: (TaggedValidationState[T], A) => ValidationResult[T]): TaggedValidationState[T] =
    seq.foldLeft(this) { (state, elem) =>
      state.pass(operation(state, elem))
    }

  /** This is for nested validations that allow mixing fail-fast and accumulate-errors validation strategies
    */
  def validate(operation: => ValidationResult[T]): TaggedValidationState[T] = pass(operation)

}
