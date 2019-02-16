package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.app.Version
import scorex.core.network.{ConnectionType, Handshake, PeerData}

/**
  * Information about peer to be stored in PeerDatabase
  *
  * @param peerData       - general information about the peer
  * @param lastSeen       - timestamp when this peer was last seen in the network
  * @param connectionType - type of connection (Incoming/Outgoing) established to this peer if any
  */
case class PeerInfo(peerData: PeerData,
                    lastSeen: Long,
                    connectionType: Option[ConnectionType] = None) {

  lazy val handshake: Handshake = Handshake(peerData, lastSeen)

}

object PeerInfo {

  def fromAddress(address: InetSocketAddress): PeerInfo = {
    val peerData = PeerData("configNode", Version.last, s"config-$address", Some(address), Seq())
    PeerInfo(peerData, 0L, None)
  }

}