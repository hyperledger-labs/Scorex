package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.TimeProvider


trait PeerDatabase {
  def isEmpty(): Boolean

  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit

  def knownPeers(): Map[InetSocketAddress, PeerInfo]

  def get(address: InetSocketAddress): Option[PeerInfo]

  def blacklistPeer(peer: InetSocketAddress, time: TimeProvider.Time): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Boolean
}

