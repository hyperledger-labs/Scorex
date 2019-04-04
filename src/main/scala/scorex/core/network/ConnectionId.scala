package scorex.core.network

import java.net.InetSocketAddress

/**
  * Wraps (remoteAddress, localAddress, direction) tuple, which allows to precisely identify peer.
  */
final case class ConnectionId(remoteAddress: InetSocketAddress,
                              localAddress: InetSocketAddress,
                              direction: ConnectionDirection) {

  // Real endpoint address we should work with depends on connection direction
  val address: InetSocketAddress = if (direction.isIncoming) localAddress else remoteAddress
}
