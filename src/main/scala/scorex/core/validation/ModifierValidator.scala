package scorex.core.validation

import scorex.core.utils.ScorexEncoder
import scorex.core.validation.ValidationResult._

/**
  * Object with helpers for the modifier validation process.
  *
  * Allows to initialize ValidationState from current ValidationSettings,
  *
  * It is designed to perform multiple checks for the same object without any transformation.
  * See the example of that
  * kind of validation in Ergo `org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor.HeaderValidator`.
  * Some other examples could also be found in `scorex.core.validation.ValidationSpec`.
  *
  * The second distinction from cats `Validated` is that we do support both fail-fast and error-accumulating validation
  * while cats `Validated` supports only accumulative approach.
  */
object ModifierValidator {

  /** Start validation in Fail-Fast mode */
  def failFast(implicit e: ScorexEncoder): ValidationState[Unit] =
    ValidationState(ModifierValidator.success, ValidationStrategy.FailFast)(e)

  /** Start validation accumulating all the errors */
  def accumulateErrors(implicit e: ScorexEncoder): ValidationState[Unit] =
    ValidationState(ModifierValidator.success, ValidationStrategy.AccumulateErrors)(e)

  /** Start tagged validation in Fail-Fast mode */
  def failFastTagged(settings: TaggedValidationRules)(implicit e: ScorexEncoder): TaggedValidationState[Unit] =
    TaggedValidationState(ModifierValidator.success, ValidationStrategy.FailFast, settings)(e)

  /** Start tagged validation accumulating all the errors */
  def accumulateErrorsTagged(settings: TaggedValidationRules)(implicit e: ScorexEncoder): TaggedValidationState[Unit] =
    TaggedValidationState(ModifierValidator.success, ValidationStrategy.AccumulateErrors, settings)(e)

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid =
    invalid(new RecoverableModifierError(errorMessage, None))

  /** report recoverable modifier error that could be fixed by later retries */
  def error(description: String, cause: Throwable): Invalid =
    invalid(new RecoverableModifierError(msg(description, cause), Option(cause)))

  /** report recoverable modifier error that could be fixed by later retries */
  def error(description: String, detail: String): Invalid =
    error(msg(description, detail))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid =
    invalid(new MalformedModifierError(errorMessage, None))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String, cause: Throwable): Invalid =
    invalid(new MalformedModifierError(msg(errorMessage, cause), Option(cause)))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(description: String, detail: String): Invalid = fatal(msg(description, detail))

  /** unsuccessful validation with a given error; also logs the error as an exception */
  def invalid(error: ModifierError): Invalid =
    Invalid(Seq(error))

  /** successful validation without payload */
  val success: Valid[Unit] = Valid(())

  private def msg(descr: String, e: Throwable): String = msg(descr, Option(e.getMessage).getOrElse(e.toString))

  private def msg(description: String, detail: String): String = s"$description: $detail"
}
