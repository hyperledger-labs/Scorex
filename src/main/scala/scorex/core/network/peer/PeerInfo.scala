package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.app.Version
import scorex.core.network.{ConnectionType, Handshake, PeerData}

/**
  * Information about peer to be stored in PeerDatabase
  */
case class PeerInfo(peerData: PeerData,
                    lastSeen: Long,
                    connectionType: Option[ConnectionType] = None) {

  lazy val handshake: Handshake = Handshake(peerData, lastSeen)

}

object PeerInfo {

  def fromAddress(address: InetSocketAddress, time: Long): PeerInfo = {
    val peerData = PeerData("configNode", Version.last, s"config-${address}", Some(address), Seq())
    PeerInfo(peerData, time, None)
  }

}