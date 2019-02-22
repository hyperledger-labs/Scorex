package scorex.core.network

/**
  * Network message to be send when nodes establish a new connection.
  * When a node creates an outgoing connection, it will immediately advertise its Handshake.
  * The remote node will respond with its Handshake.
  * No further communication is possible until both peers have exchanged their handshakes.
  *
  * @param peerSpec - general (declared) information about peer
  * @param time     - handshake time
  */
case class Handshake(peerSpec: PeerSpec, time: Long)
