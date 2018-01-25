package scorex.core.network.peer

import java.net.InetSocketAddress

case class PeerInfo(lastSeen: Long, nodeName: Option[String] = None)

trait PeerDatabase {

  def isEmpty():Boolean

  def addOrUpdateKnownPeer(peer: InetSocketAddress, peerInfo: PeerInfo): Unit

  def knownPeers(forSelf: Boolean): Map[InetSocketAddress, PeerInfo]

  def blacklistPeer(peer: InetSocketAddress): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean
}

