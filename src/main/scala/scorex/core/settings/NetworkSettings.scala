package scorex.core.settings

import java.net.InetSocketAddress

import scorex.core.app.Version

case class NetworkSettings(nodeName: String,
                           nodeNonce: Long,
                           addedMaxDelay: Option[Int],
                           networkChunkSize: Int,
                           localOnly: Boolean,
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: String,
                           maxConnections: Int,
                           connectionTimeout: Int,
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[Int],
                           upnpDiscoverTimeout: Option[Int],
                           port: Int,
                           declaredAddress: Option[String],
                           handshakeTimeout: Int,
                           appVersion: Version,
                           agentName: String)
