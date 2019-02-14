package scorex.core.network.peer

import scorex.core.network.{ConnectionType, Handshake, PeerData}

/**
  * Information about peer to be stored in PeerDatabase
  */
case class PeerInfo(peerData: PeerData,
                    lastSeen: Long,
                    connectionType: Option[ConnectionType] = None) {

  lazy val handshake: Handshake = Handshake(peerData, lastSeen)

}