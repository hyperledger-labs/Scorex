package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.app.Version
import scorex.core.network.{ConnectionDirection, PeerSpec}

/**
  * Information about peer to be stored in PeerDatabase
  *
  * @param peerSpec       - general information about the peer
  * @param lastSeen       - timestamp when this peer was last seen in the network
  * @param connectionType - type of connection (Incoming/Outgoing) established to this peer if any
  */
case class PeerInfo(peerSpec: PeerSpec,
                    lastSeen: Long,
                    connectionType: Option[ConnectionDirection] = None)

object PeerInfo {

  /**
    * Create peer info from address only, when we don't know other fields
    * (e.g. we got this information from config or from API)
    */
  def fromAddress(address: InetSocketAddress): PeerInfo = {
    val peerSpec = PeerSpec("unknown", Version.initial, s"unknown-$address", Some(address), Seq())
    PeerInfo(peerSpec, 0L, None)
  }

}