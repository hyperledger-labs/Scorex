package scorex.core.network.peer

import java.net.{InetSocketAddress, NetworkInterface, URI}

import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTime

import scala.collection.mutable
import scala.collection.JavaConverters._


//todo: persistence
class PeerDatabaseImpl(settings: NetworkSettings, filename: Option[String]) extends PeerDatabase {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, Long]()

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelistPersistence.get(address).map { dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
    }.getOrElse(peerInfo)
    whitelistPersistence.put(address, updatedPeerInfo)
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> NetworkTime.time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] = {
    if (excludeSelf) {
      // todo: we should improve `NetworkSettings`, so that `bindAddress` is already parsed as
      // todo: `InetSocketAddress` instead of `String`.
      val bindInetSocketAddress = new InetSocketAddress(settings.bindAddress, settings.port)

      val localAddresses = if (bindInetSocketAddress.getAddress.isAnyLocalAddress) {
        NetworkInterface.getNetworkInterfaces.asScala
          .flatMap(_.getInetAddresses.asScala
            .map(a => new InetSocketAddress(a, bindInetSocketAddress.getPort)))
          .toSet
      } else Set(bindInetSocketAddress)


      // todo: make `declaredAddress` be parsed as `InetSocketAddress`, as discussed above.
      val declaredInetSocketAddress = settings.declaredAddress map { a =>
        val uri = new URI(s"my://$a")
        new InetSocketAddress(uri.getHost, uri.getPort)
      }

      val excludedAddresses = localAddresses ++ declaredInetSocketAddress.toSet
      knownPeers(false).filterKeys(!excludedAddresses(_))
    } else {
      whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap
    }

  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty
}
