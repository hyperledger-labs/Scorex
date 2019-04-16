package scorex.core.network

import java.net.InetSocketAddress

/**
  * Wraps (remoteAddress, localAddress, direction) tuple, which allows to precisely identify peer.
  */
final case class ConnectionId(remoteAddress: InetSocketAddress,
                              localAddress: InetSocketAddress,
                              direction: ConnectionDirection) {

  // Real endpoint address we expect to work with
  val address: InetSocketAddress = if (direction.isIncoming) localAddress else remoteAddress

  override def toString: String = s"ConnectionId(${address.toString}, direction=$direction)"
}
