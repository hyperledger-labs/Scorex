package scorex.util

import org.scalatest.{FlatSpec, Matchers}

class ScorexLoggingSpec extends FlatSpec with Matchers with ScorexLogging {

  "Logger" should "evaluate messages only if the respective log level is enabled" in {
    var i = 0
    log.info(s"Info level message, should be evaluated ${i = 1} i = $i")
    i shouldBe 1
    log.trace(s"Trace level message, should not be evaluated ${i = 2} i = $i")
    i shouldBe 1
  }

}