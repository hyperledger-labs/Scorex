package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

trait PeerDatabase {

  def get(peer: InetSocketAddress): Option[PeerInfo]

  def isEmpty: Boolean

  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit

  def peerSeen(peerInfo: PeerInfo): Unit

  def knownPeers: Map[InetSocketAddress, PeerInfo]

  def addToBlacklist(address: InetSocketAddress, penaltyType: PenaltyType): Unit

  def removeFromBlacklist(address: InetAddress): Unit

  def blacklistedPeers: Seq[InetAddress]

  def isBlacklisted(address: InetAddress): Boolean

  def remove(address: InetSocketAddress): Unit

}
