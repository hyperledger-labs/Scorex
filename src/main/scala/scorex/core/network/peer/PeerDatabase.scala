package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

trait PeerDatabase {

  def get(peer: InetSocketAddress): Option[PeerInfo]

  def isEmpty:Boolean

  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit

  def knownPeers: Map[InetSocketAddress, PeerInfo]

  def addToBlacklist(address: InetSocketAddress, banTime: Long): Unit

  def removeFromBlacklist(address: InetSocketAddress): Unit

  def blacklistedPeers: Seq[InetAddress]

  def isBlacklisted(address: InetSocketAddress): Boolean

  def remove(address: InetSocketAddress): Unit

}
