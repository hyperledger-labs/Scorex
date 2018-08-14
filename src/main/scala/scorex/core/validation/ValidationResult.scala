package scorex.core.validation

import akka.http.scaladsl.server.Route
import io.circe.{ACursor, DecodingFailure}
import scorex.core.api.http.ApiError
import scorex.core.validation.ValidationResult.{Invalid, Valid}

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/** Base trait for the result of validation
  */
sealed trait ValidationResult[+T] {

  /** Whether validation was successful
    */
  def isValid: Boolean

  /** Error description for the unsuccessful validation
    */
  def message: String

  /** Errors for the unsuccessful validation
    */
  def errors: Seq[ModifierError]

  /** Payload for the successful validation
    */
  def payload: Option[T]

  /** Replace payload with the new one, discarding current payload value
    */
  def apply[R](payload: R): ValidationResult[R]

  /** Map payload if validation is successful
    */
  def map[R](f: T => R): ValidationResult[R]

  /** Convert validation result to Try
    */
  def toTry: Try[T]

  /** Create completed Future with validation result
    */
  def toFuture: Future[T] = Future.fromTry(toTry)

  /** Convert validation result to circe json decoding result
    */
  def toDecoderResult(implicit cursor: ACursor): Either[DecodingFailure, T] = this match {
    case Valid(value) => Right(value)
    case Invalid(_) => Left(DecodingFailure(message, cursor.history))
  }

  /** Convert validation result to akka http route
    */
  def toApi(onSuccess: T => Route): Route = this match {
    case Valid(value) => onSuccess(value)
    case Invalid(_) => ApiError.BadRequest(message)
  }

}

object ValidationResult {

  /** Successful validation result
    */
  final case class Valid[+T](value: T) extends ValidationResult[T] {
    def isValid: Boolean = true
    def message: String = "OK"
    def errors: Seq[ModifierError] = Seq.empty
    def payload: Option[T] = Option(value)
    def apply[R](payload: R): ValidationResult[R] = Valid(payload)
    def map[R](f: T => R): ValidationResult[R] = Valid(f(value))
    def toTry: Try[T] = Success(value)
  }

  /** Unsuccessful validation result
    */
  final case class Invalid(errors: Seq[ModifierError]) extends ValidationResult[Nothing] {
    def isValid: Boolean = false
    def isFatal: Boolean = errors.exists(_.isFatal)
    def message: String = "Validation errors: " + errors.mkString(" | ")
    def payload: Option[Nothing] = None
    def apply[R](payload: R): Invalid = this
    def map[R](f: Nothing => R): Invalid = this

    def accumulateErrors[T](next: ValidationResult[T]): ValidationResult[T] = {
      next match {
        case Valid(_) => this
        case Invalid(e2) => Invalid(errors ++ e2)
      }
    }

    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    override lazy val toTry: Failure[Nothing] = {
      if (errors.size == 1) Failure(errors.head.toThrowable) else Failure(MultipleErrors(errors))
    }

  }

  /** Shorthand to get the result of validation */
  implicit def fromValidationState[R](state: ValidationState[R]): ValidationResult[R] = state.result

}

