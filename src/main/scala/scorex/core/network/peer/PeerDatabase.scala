package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.TimeProvider

trait PeerDatabase {

  def get(peer: InetSocketAddress): Option[PeerInfo]

  def isEmpty:Boolean

  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit

  def knownPeers: Map[InetSocketAddress, PeerInfo]

  def addToBlacklist(address: InetSocketAddress, banTime: Long): Unit

  def removeFromBlacklist(address: InetSocketAddress): Unit

  def blacklistedPeers: Seq[InetSocketAddress]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Unit

}
