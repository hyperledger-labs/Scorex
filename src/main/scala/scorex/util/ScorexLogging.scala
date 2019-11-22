package scorex.util

import com.typesafe.scalalogging.StrictLogging

trait ScorexLogging extends StrictLogging {
  @inline protected def log = logger
}