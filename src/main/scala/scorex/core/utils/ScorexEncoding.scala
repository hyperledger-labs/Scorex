package scorex.core.utils

/**
  * Trait with bytes to string encoder
  * TODO extract to ScorexUtils project
  */
trait ScorexEncoding {
  implicit val encoder: ScorexEncoder = ScorexEncoder.default
}
