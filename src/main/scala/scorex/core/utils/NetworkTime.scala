package scorex.core.utils

import java.net.InetAddress
import java.util.concurrent.atomic.AtomicLong

import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object NetworkTime {
  def localWithOffset(offset: Long): Long = System.currentTimeMillis() + offset
  type Offset = Long
}

case class NetworkTimeProviderSettings(server: String, updateEvery: FiniteDuration, timeout: FiniteDuration)

class NetworkTimeProvider(ntpSettings: NetworkTimeProviderSettings)(implicit ec: ExecutionContext)
  extends TimeProvider with scorex.util.ScorexLogging {

  private val lastUpdate = new AtomicLong(0)
  private var offset = new AtomicLong(0)
  private val client = new NTPUDPClient()
  client.setDefaultTimeout(ntpSettings.timeout.toMillis.toInt)
  client.open()

  override def time(): TimeProvider.Time = {
    checkUpdateRequired()
    NetworkTime.localWithOffset(offset.get())
  }

  private def updateOffset(): Future[NetworkTime.Offset] = Future {
    val info = client.getTime(InetAddress.getByName(ntpSettings.server))
    info.computeDetails()
    info.getOffset
  }

  private def checkUpdateRequired(): Unit = {
    val time = NetworkTime.localWithOffset(offset.get())
    // set lastUpdate to current time so other threads won't start to update it
    val lu = lastUpdate.getAndSet(time)
    if (time > lu + ntpSettings.updateEvery.toMillis) {
      // time to update offset
      updateOffset().onComplete {
        case Success(newOffset) =>
          offset.set(newOffset)
          log.info("New offset adjusted: " + offset)
          lastUpdate.set(time)
        case Failure(e) =>
          log.warn("Problems with NTP: ", e)
          lastUpdate.compareAndSet(time, lu)
      }
    } else {
      // No update required. Set lastUpdate back to it's initial value
      lastUpdate.compareAndSet(time, lu)
    }
  }
}