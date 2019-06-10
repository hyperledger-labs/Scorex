package scorex.core.network

import java.net.InetSocketAddress

/**
  * Wraps (remoteAddress, localAddress, direction) tuple, which allows to precisely identify peer.
  */
final case class ConnectionId(remoteAddress: InetSocketAddress,
                              localAddress: InetSocketAddress,
                              direction: ConnectionDirection) {

  override def toString: String =
    s"ConnectionId(remote=${remoteAddress.toString}, local=${localAddress.toString}, direction=$direction)"
}
