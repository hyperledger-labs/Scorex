package scorex.core.validation

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/** Base trait for the result of validation
  */
sealed trait ValidationResult {

  def isValid: Boolean

  def ++(next: ValidationResult): ValidationResult

  def toTry: Try[Unit]

  def toFuture: Future[Unit] = Future.fromTry(toTry)

}

object ValidationResult {

  type Valid = ValidationResult.Valid.type

  /** Successful validation result
    */
  final case object Valid extends ValidationResult {
    def isValid: Boolean = true
    def ++(next: ValidationResult): ValidationResult = next
    def toTry: Try[Unit] = Success(())
  }

  /** Unsuccessful validation result
    */
  final case class Invalid(errors: Seq[ModifierError]) extends ValidationResult {
    def isValid: Boolean = false
    def isFatal: Boolean = errors.exists(_.isFatal)

    def ++(next: ValidationResult): ValidationResult = {
      next match {
        case Valid => this
        case Invalid(e2) => Invalid(errors ++ e2)
      }
    }

    lazy val toTry: Try[Unit] = Failure(ValidationErrors(errors))
  }

}

@SuppressWarnings(Array("org.wartremover.warts.Null"))
case class ValidationErrors(errors: Seq[ModifierError])
  extends Exception(errors.mkString(" | "), errors.headOption.map(_.toThrowable).orNull) {
  def isFatal: Boolean = errors.exists(_.isFatal)
}
