package scorex.core.network

/**
  * Custom exception to distinguish malicious behaviour of external peers from non-adversarial network issues
  *
  * @param message - exception message
  */
case class MaliciousBehaviorException(message: String) extends Exception(message)