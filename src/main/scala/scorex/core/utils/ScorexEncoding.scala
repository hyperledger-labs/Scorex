package scorex.core.utils

import scorex.core.crypto.encode.{Base16, BytesEncoder}

/**
  * Trait with bytes to string encoder
  * TODO extract to ScorexUtils project
  */
trait ScorexEncoding {
  implicit val encoder: BytesEncoder = Base16
}
