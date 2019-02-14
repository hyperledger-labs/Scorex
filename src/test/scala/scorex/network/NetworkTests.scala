package scorex.network

import java.net.InetSocketAddress

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.app.Version
import scorex.core.network.PeerData
import scorex.core.network.peer.PeerInfo
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, TimeProvider}
import scala.concurrent.ExecutionContext.Implicits.global

class NetworkTests extends FlatSpec with Matchers {

  protected val settings: ScorexSettings = ScorexSettings.read(None)
  protected val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  protected def currentTime(): TimeProvider.Time = timeProvider.time()

  protected def getPeerInfo(address: InetSocketAddress): PeerInfo = {
    val data = PeerData("full node", Version.last, address.toString, Some(address), Seq())
    PeerInfo(data, currentTime(), None)
  }

}