package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.NetworkTime


trait PeerDatabase {
  def isEmpty():Boolean

  def addOrUpdateKnownPeer(peer: InetSocketAddress, peerInfo: PeerInfo): Unit

  def knownPeers(): Map[InetSocketAddress, PeerInfo]

  def blacklistPeer(peer: InetSocketAddress, time: NetworkTime.Time): Unit

  def blacklistedPeers(): Seq[String]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Boolean
}

