package scorex.core.utils

import scorex.crypto.encode.{Base16, BytesEncoder}
import scorex.utils.ScryptoLogging

trait ScorexLogging extends ScryptoLogging {
  override implicit val encoder: BytesEncoder = Base16
}
