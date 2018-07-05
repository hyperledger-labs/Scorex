package scorex.core.validation

import akka.http.scaladsl.server.Route
import io.circe.{ACursor, Decoder, DecodingFailure}
import scorex.core.api.http.ApiError
import scorex.core.validation.ValidationResult.{Invalid, Valid}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/** Base trait for the result of validation
  */
sealed trait ValidationResult {

  def isValid: Boolean

  def message: String

  def errors: Seq[ModifierError]

  def ++(next: ValidationResult): ValidationResult

  def toTry[T](success: T): Try[T]

  def toTry: Try[Unit] = toTry(())

  def toFuture: Future[Unit] = Future.fromTry(toTry)

  def toDecoderResult[T](value: T)(implicit cursor: ACursor): Decoder.Result[T] = this match {
    case Valid => Right(value)
    case Invalid(_) => Left(DecodingFailure(message, cursor.history))
  }

  def toApi(onSuccess: => Route): Route = this match {
    case Valid => onSuccess
    case Invalid(_) => ApiError.BadRequest(message)
  }

}

object ValidationResult {

  type Valid = ValidationResult.Valid.type

  /** Successful validation result
    */
  final case object Valid extends ValidationResult {
    def isValid: Boolean = true
    def message: String = "OK"
    def errors: Seq[ModifierError] = Seq.empty
    def ++(next: ValidationResult): ValidationResult = next
    def toTry[T](success: T): Try[T] = Success(success)
  }

  /** Unsuccessful validation result
    */
  final case class Invalid(errors: Seq[ModifierError]) extends ValidationResult {
    def isValid: Boolean = false
    def isFatal: Boolean = errors.exists(_.isFatal)
    def message: String = "Validation errors: " + errors.mkString(" | ")

    def ++(next: ValidationResult): ValidationResult = {
      next match {
        case Valid => this
        case Invalid(e2) => Invalid(errors ++ e2)
      }
    }

    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    override lazy val toTry: Failure[Nothing] = {
      if (errors.size == 1) Failure(errors.head.toThrowable) else Failure(MultipleErrors(errors))
    }

    def toTry[T](success: T): Failure[Nothing] = toTry
  }

}

