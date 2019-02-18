package scorex.core.network

import java.net.InetSocketAddress

import scorex.core.app.Version

/**
  * Network message to be send when nodes establish a new connection.
  * When a node creates an outgoing connection, it will immediately advertise its Handshake.
  * The remote node will respond with its Handshake.
  * No further communication is possible until both peers have exchanged their handshakes.
  *
  * @param peerData - general (declared) information about peer
  * @param time     - handshake time
  */
case class Handshake(peerData: PeerData, time: Long)

object Handshake {

  // todo do we need it?
  def apply(agentName: String,
            protocolVersion: Version,
            nodeName: String,
            declaredAddress: Option[InetSocketAddress],
            features: Seq[PeerFeature],
            time: Long): Handshake = {
    val peerData = PeerData(agentName, protocolVersion, nodeName, declaredAddress, features)
    Handshake(peerData, time)
  }

}