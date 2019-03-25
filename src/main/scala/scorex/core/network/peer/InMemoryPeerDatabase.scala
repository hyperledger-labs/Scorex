package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.settings.ScorexSettings
import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

/**
  * In-memory peer database implementation supporting temporal blacklisting.
  */
final class InMemoryPeerDatabase(settings: ScorexSettings, timeProvider: TimeProvider)
  extends PeerDatabase with ScorexLogging {

  private var peers = Map.empty[InetSocketAddress, PeerInfo]

  private var blacklist = Map.empty[InetSocketAddress, TimeProvider.Time]

  override def get(peer: InetSocketAddress): Option[PeerInfo] = peers.get(peer)

  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    if (!peerInfo.peerSpec.declaredAddress.exists(isBlacklisted)) {
      peerInfo.peerSpec.address.foreach { address =>
        peers += address -> peerInfo
      }
    }
  }

  override def addToBlacklist(address: InetSocketAddress): Unit = {
    peers -= address
    if (!blacklist.contains(address)) {
      log.info(s"$address blacklisted")
      blacklist += address -> timeProvider.time()
    }
  }

  override def removeFromBlacklist(address: InetSocketAddress): Unit = {
    log.info(s"$address is to be removed from blacklist")
    blacklist -= address
  }

  override def remove(address: InetSocketAddress): Unit = {
    log.info(s"$address is to be removed from known peers")
    peers -= address
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.get(address).exists { banTime =>
      val stillBanned = timeProvider.time() - banTime < settings.network.misbehaviorBanTime.toMillis
      if (!stillBanned) removeFromBlacklist(address)
      stillBanned
    }
  }

  override def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  override def blacklistedPeers: Seq[InetSocketAddress] = blacklist.keys.toSeq

  override def isEmpty: Boolean = peers.isEmpty

}
