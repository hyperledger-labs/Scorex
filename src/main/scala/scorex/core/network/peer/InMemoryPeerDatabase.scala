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

  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    if (!peerInfo.declaredAddress.exists(isBlacklisted)) {
      log.info(s"$peerInfo added to known peers")
      peerInfo.declaredAddress.foreach { address =>
        val updatedPeerInfo = peers.get(address).fold(peerInfo) { dbPeerInfo =>
          val nodeNameOpt = peerInfo.nodeName orElse dbPeerInfo.nodeName
          val connTypeOpt = peerInfo.connectionType orElse dbPeerInfo.connectionType
          val peerFeatures = if (dbPeerInfo.features.nonEmpty && peerInfo.features.isEmpty) {
            dbPeerInfo.features
          } else {
            peerInfo.features
          }
          PeerInfo(peerInfo.lastSeen, peerInfo.declaredAddress, nodeNameOpt, connTypeOpt, peerFeatures)
        }
        peers += address -> updatedPeerInfo
      }
    }
  }

  override def addToBlacklist(address: InetSocketAddress): Unit = {
    log.info(s"$address blacklisted")
    peers -= address
    if (!isBlacklisted(address)) blacklist += address -> timeProvider.time()
  }

  override def removeFromBlacklist(address: InetSocketAddress): Unit = {
    log.info(s"$address removed from blacklist")
    blacklist -= address
  }

  override def remove(address: InetSocketAddress): Unit = {
    log.info(s"$address removed from known peers")
    peers -= address
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.get(address).exists { banTime =>
      val stillBanned = timeProvider.time() - banTime < settings.network.misbehavingBanTime
      if (!stillBanned) removeFromBlacklist(address)
      stillBanned
    }
  }

  override def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  override def blacklistedPeers: Seq[InetSocketAddress] = blacklist.keys.toSeq

  override def isEmpty: Boolean = peers.isEmpty

}
