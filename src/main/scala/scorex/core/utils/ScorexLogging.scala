package scorex.core.utils

import com.typesafe.scalalogging.StrictLogging
import scorex.crypto.encode.{Base16, BytesEncoder}

// TODO just extend from scrypto logging after the next release
trait ScorexLogging  extends StrictLogging {
  @inline protected def log = logger

  implicit val encoder: BytesEncoder = Base16
}
