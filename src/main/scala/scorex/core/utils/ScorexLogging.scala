package scorex.core.utils

import com.typesafe.scalalogging.StrictLogging
import scorex.crypto.encode.{Base16, BytesEncoder}

/**
* TODO extract to ScorexUtils project
*/
trait ScorexLogging extends StrictLogging {
  @inline protected def log = logger
}
