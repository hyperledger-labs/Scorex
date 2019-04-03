package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.settings.ScorexSettings
import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

/**
  * In-memory peer database implementation supporting temporal blacklisting.
  */
final class InMemoryPeerDatabase(settings: ScorexSettings, timeProvider: TimeProvider)
  extends PeerDatabase with ScorexLogging {

  private val defaultBanDuration = settings.network.misbehaviorBanDuration.toMillis

  private var peers = Map.empty[InetSocketAddress, PeerInfo]

  // banned peer ip -> ban expiration timestamp
  private var blacklist = Map.empty[InetAddress, TimeProvider.Time]

  override def get(peer: InetSocketAddress): Option[PeerInfo] = peers.get(peer)

  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    if (!peerInfo.peerSpec.declaredAddress.exists(x => isBlacklisted(x.getAddress))) {
      peerInfo.peerSpec.address.foreach { address =>
        peers += address -> peerInfo
      }
    }
  }

  override def addToBlacklist(address: InetSocketAddress,
                              banDuration: Long = defaultBanDuration): Unit = {
    peers -= address
    if (!blacklist.contains(address.getAddress)) blacklist += address.getAddress -> (timeProvider.time() + banDuration)
    else log.warn(s"$address is already blacklisted")
  }

  override def removeFromBlacklist(address: InetAddress): Unit = {
    log.info(s"$address removed from blacklist")
    blacklist -= address
  }

  override def remove(address: InetSocketAddress): Unit = {
    peers -= address
  }

  override def isBlacklisted(address: InetAddress): Boolean =
    blacklist.get(address).exists(checkBanned(address, _))

  override def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  override def blacklistedPeers: Seq[InetAddress] = blacklist
    .map { case (address, bannedTil) =>
      checkBanned(address, bannedTil)
      address
    }
    .toSeq

  override def isEmpty: Boolean = peers.isEmpty

  private def checkBanned(address: InetAddress, bannedTil: Long): Boolean = {
    val stillBanned = timeProvider.time() < bannedTil
    if (!stillBanned) removeFromBlacklist(address)
    stillBanned
  }

}
