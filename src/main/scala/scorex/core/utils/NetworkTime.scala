package scorex.core.utils

import java.net.InetAddress

import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Left

object NetworkTime {
  def localWithOffset(offset: Long): Long = System.currentTimeMillis() + offset

  type Offset = Long
  type Time = Long
}

protected case class NetworkTime(offset: NetworkTime.Offset, lastUpdate: NetworkTime.Time)

case class NetworkTimeProviderSettings(server: String, updateEvery: FiniteDuration, timeout: FiniteDuration)

class NetworkTimeProvider(ntpSettings: NetworkTimeProviderSettings) extends ScorexLogging {

  private type State = Either[(NetworkTime, Future[NetworkTime]), NetworkTime]

  private def updateOffSet(): Option[NetworkTime.Offset] = {
    val client = new NTPUDPClient()
    client.setDefaultTimeout(ntpSettings.timeout.toMillis.toInt)
    try {
      client.open()

      val info = client.getTime(InetAddress.getByName(ntpSettings.server))
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

  private def timeAndState(currentState: State): (NetworkTime.Time, State) = {
    currentState match {
      case Right(nt) =>
        val time = NetworkTime.localWithOffset(nt.offset)
        val state = if (time > nt.lastUpdate + ntpSettings.updateEvery.toMillis) {
          Left(nt -> Future(updateOffSet()).map { mbOffset =>
            log.info("New offset adjusted: " + mbOffset)
            val offset = mbOffset.getOrElse(nt.offset)
            NetworkTime(offset, NetworkTime.localWithOffset(offset))
          })
        } else {
          Right(nt)
        }
        (time, state)

      case Left((nt, f)) =>
        if (f.isCompleted) {
          val nnt = Await.result(f, 10.seconds)
          NetworkTime.localWithOffset(nnt.offset) -> Right(nnt)
        } else NetworkTime.localWithOffset(nt.offset) -> Left(nt -> f)
    }
  }

  private var state: State = Right(NetworkTime(0L, 0L))

  def time(): NetworkTime.Time = {
    val t = timeAndState(state)
    state = t._2
    t._1
  }
}