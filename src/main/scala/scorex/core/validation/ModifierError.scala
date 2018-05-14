package scorex.core.validation

/** Base trait for errors that were occurred during NodeView Modifier validation
  */
trait ModifierError {
  def message: String
  def isFatal: Boolean
  def toThrowable: Throwable
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
