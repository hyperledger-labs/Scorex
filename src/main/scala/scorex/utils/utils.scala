package scorex

import java.security.SecureRandom

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

package object utils {

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration,
                            delay: FiniteDuration = 100.milliseconds)(fn: => T): T = {
    Try {
      fn
    } match {
      case Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay)(fn)
      case Failure(e) => throw e
    }
  }

  def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }
}
