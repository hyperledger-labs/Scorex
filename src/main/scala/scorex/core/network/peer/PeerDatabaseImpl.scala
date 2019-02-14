package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

import scala.collection.mutable

//todo: persistence
class PeerDatabaseImpl(filename: Option[String]) extends PeerDatabase with ScorexLogging {

  private val knownPeersVar = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, TimeProvider.Time]()

  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit = {
    log.trace(s"Add or Update known peer: $peerInfo")

    peerInfo.peerData.address.foreach { address =>
      knownPeersVar.put(address, peerInfo)
    }
  }

  override def blacklistPeer(address: InetSocketAddress, time: TimeProvider.Time): Unit = {
    log.warn(s"Black list peer: ${address.toString}")
    knownPeersVar.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(): Map[InetSocketAddress, PeerInfo] = {
    log.trace(s"Known peers:  ${knownPeersVar.toMap}")
    knownPeersVar.keys.flatMap(k => knownPeersVar.get(k).map(v => k -> v)).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = knownPeersVar.isEmpty

  override def remove(address: InetSocketAddress): Boolean = {
    log.trace(s"Remove peer: ${address.toString}")
    knownPeersVar.remove(address).nonEmpty
  }
}
