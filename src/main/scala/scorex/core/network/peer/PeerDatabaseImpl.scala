package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.settings.NetworkSettings

import scala.collection.mutable


//todo: persistence
class PeerDatabaseImpl(settings: NetworkSettings, filename: Option[String]) extends PeerDatabase {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, Long]()

  private lazy val ownNonce = settings.nodeNonce

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
    if (!isBlacklisted(address)) blacklist += address.getHostName -> System.currentTimeMillis()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (if (excludeSelf) knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce.getOrElse(-1))
     else whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v))).toMap

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty
}
