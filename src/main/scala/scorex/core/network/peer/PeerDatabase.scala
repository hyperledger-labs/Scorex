package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.NetworkTime

case class PeerInfo(lastSeen: Long, nonce: Option[Long] = None, nodeName: Option[String] = None)

trait PeerDatabase {

  def isEmpty():Boolean

  def addOrUpdateKnownPeer(peer: InetSocketAddress, peerInfo: PeerInfo): Unit

  def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo]

  def blacklistPeer(peer: InetSocketAddress, time: NetworkTime.Time): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean
}

