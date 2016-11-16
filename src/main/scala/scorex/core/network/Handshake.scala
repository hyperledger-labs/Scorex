package scorex.core.network

import java.net.InetSocketAddress

import scorex.core.app.ApplicationVersion


case class Handshake(applicationName: String,
                     applicationVersion: ApplicationVersion,
                     nodeName: String,
                     nodeNonce: Long,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long) {

  require(Option(applicationName).isDefined)
  require(Option(applicationVersion).isDefined)

}