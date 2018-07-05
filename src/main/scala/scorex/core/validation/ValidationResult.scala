package scorex.core.validation

import akka.http.scaladsl.server.Route
import io.circe.{ACursor, Decoder, DecodingFailure}
import scorex.core.api.http.ApiError
import scorex.core.validation.ValidationResult.{Invalid, Valid}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/** Base trait for the result of validation
  */
sealed trait ValidationResult[+T] {

  def isValid: Boolean

  def message: String

  def errors: Seq[ModifierError]

  def payload: Option[T]

  def map[R](f: T => R): ValidationResult[R]

  def toTry: Try[T]

  def toFuture: Future[T] = Future.fromTry(toTry)

  def toDecoderResult(implicit cursor: ACursor): Either[DecodingFailure, T] = this match {
    case Valid(value) => Right(value)
    case Invalid(_) => Left(DecodingFailure(message, cursor.history))
  }

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

}

