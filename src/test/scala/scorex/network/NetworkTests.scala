package scorex.network

import java.net.InetSocketAddress

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.app.Version
import scorex.core.network.PeerSpec
import scorex.core.network.peer.PeerInfo
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, TimeProvider}
import scala.concurrent.ExecutionContext.Implicits.global

class NetworkTests extends AnyFlatSpec with Matchers {

  protected val settings: ScorexSettings = ScorexSettings.read(None)
  protected val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  protected def currentTime(): TimeProvider.Time = timeProvider.time()

  protected def getPeerInfo(address: InetSocketAddress, nameOpt: Option[String] = None): PeerInfo = {
    val data = PeerSpec("full node", Version.last, nameOpt.getOrElse(address.toString), Some(address), Seq())
    PeerInfo(data, currentTime(), None)
  }

}