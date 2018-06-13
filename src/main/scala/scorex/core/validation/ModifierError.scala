package scorex.core.validation

/** Base trait for errors that were occurred during NodeView Modifier validation
  */
trait ModifierError {
  def message: String
  def isFatal: Boolean
  def toThrowable: Throwable

  def description: String = {
    val fatality = if (isFatal) "fatally" else "non-fatal"
    s"Modifier Validation failed $fatality: $message"
  }
}

/** Permanent modifier error that could not be recovered in future even after any history updates
  */
case class MalformedModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = true
  def toThrowable: Throwable = this
}

/** Temporary modifier error that may be recovered in future after some history updates
  */
case class RecoverableModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = false
  def toThrowable: Throwable = this
}


/** Composite error class that can hold more than one modifier error inside. This was not made a `ModifierError` instance
  * intentionally to prevent nesting `MultipleErrors` to `MultipleErrors`
  */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
case class MultipleErrors(errors: Seq[ModifierError])
     extends Exception(errors.mkString(" | "), errors.headOption.map(_.toThrowable).orNull) {
  def isFatal: Boolean = errors.exists(_.isFatal)
}
