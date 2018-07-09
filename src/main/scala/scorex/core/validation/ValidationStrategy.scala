package scorex.core.validation

/** The strategy indicates are we going to perform fail-fast or error-accumulative validation.
  * These two could be also mixed by nested validations.
  */
sealed abstract class ValidationStrategy(val isFailFast: Boolean) {
  implicit def asImplicit: ValidationStrategy = this
}

object ValidationStrategy {

  object AccumulateErrors extends ValidationStrategy(false)

  object FailFast extends ValidationStrategy(true)

}
