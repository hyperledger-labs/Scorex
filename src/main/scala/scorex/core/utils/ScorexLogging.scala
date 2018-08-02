package scorex.core.utils

import com.typesafe.scalalogging.StrictLogging

/**
* TODO extract to ScorexUtils project
*/
trait ScorexLogging extends StrictLogging {
  @inline protected def log = logger
}
