package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

import scala.collection.mutable

//todo: persistence
class PeerDatabaseImpl(filename: Option[String]) extends PeerDatabase with ScorexLogging {

  private val knownPeersMap = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, TimeProvider.Time]()

  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    log.trace(s"Add or Update known peer: $peerInfo")

    peerInfo.peerData.address.foreach { address =>
      knownPeersMap.put(address, peerInfo)
    }
  }

  override def blacklistPeer(address: InetSocketAddress, time: TimeProvider.Time): Unit = {
    log.warn(s"Black list peer: ${address.toString}")
    knownPeersMap.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(): Map[InetSocketAddress, PeerInfo] = {
    log.trace(s"Known peers:  ${knownPeersMap.toMap}")
    knownPeersMap.keys.flatMap(k => knownPeersMap.get(k).map(v => k -> v)).toMap
  }

  override def get(address: InetSocketAddress): Option[PeerInfo] = {
    knownPeersMap.get(address)
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = knownPeersMap.isEmpty

  override def remove(address: InetSocketAddress): Boolean = {
    log.trace(s"Remove peer: ${address.toString}")
    knownPeersMap.remove(address).nonEmpty
  }
}
