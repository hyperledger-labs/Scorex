package scorex.core.app

import java.net.InetSocketAddress

import scorex.core.network.{PeerFeature, UPnPGateway}
import scorex.core.network.message.MessageSpec
import scorex.core.utils.TimeProvider

case class ScorexContext(messageSpecs: Seq[MessageSpec[_]],
                         features: Seq[PeerFeature],
                         upnpGateway: Option[UPnPGateway],
                         timeProvider: TimeProvider,
                         externalNodeAddress: Option[InetSocketAddress])
