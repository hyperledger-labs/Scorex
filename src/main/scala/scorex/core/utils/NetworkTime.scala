package scorex.core.utils

import java.net.InetAddress

import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Left

protected trait NetworkTime extends ScorexLogging {

  val offset: NetworkTime.Offset
  val lastUpdate: NetworkTime.Time

  protected def localWithOffset() = System.currentTimeMillis() + offset
}

object NetworkTime extends ScorexLogging {
  type Offset = Long
  type Time = Long

  private type State = Either[(NetworkTime, Future[NetworkTime]), NetworkTime]

  private val TimeTillUpdate = 1000 * 60 * 30L
  // 30 minutes
  private val NtpServer = "pool.ntp.org"

  private def updateOffSet(): Option[Offset] = {
    val client = new NTPUDPClient()
    client.setDefaultTimeout(10000)
    try {
      client.open()

      val info = client.getTime(InetAddress.getByName(NtpServer))
      info.computeDetails()
      Option(info.getOffset)
    } catch {
      case t: Throwable =>
        log.warn("Problems with NTP: ", t)
        None
    } finally {
      client.close()
    }
  }

  private def timeAndState(currentState: State): (Time, State) = {
    currentState match {
      case Right(nt) =>
        val time = nt.localWithOffset()
        val state = if (time > nt.lastUpdate + TimeTillUpdate) {
          Left(nt -> Future(updateOffSet()).map { mbOffset =>
            log.info("New offset adjusted: " + mbOffset)
            new NetworkTime {
              override val offset: Offset = mbOffset.getOrElse(nt.offset)
              override val lastUpdate: Time = this.localWithOffset()
            }
          })
        } else {
          Right(nt)
        }
        (time, state)

      case Left((nt, f)) =>
        if (f.isCompleted) {
          val nnt = Await.result(f, 10.seconds)
          nnt.localWithOffset() -> Right(nnt)
        } else nt.localWithOffset() -> Left(nt -> f)
    }
  }

  private var state: State = Right(new NetworkTime {
    override val lastUpdate: Time = 0L
    override val offset: Offset = 0L
  })

  def time(): Time = {
    val t = timeAndState(state)
    state = t._2
    t._1
  }
}