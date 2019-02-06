package scorex.core.network

import scorex.core.network.message.MessageSpec
import scorex.core.network.peer.ConnectedPeer

import scala.reflect.ClassTag

// Messages shared by NetworkController, PeerSynchronizer and NodeViewSynchronizer
object NetworkControllerSharedMessages {
  object ReceivableMessages {
    case class DataFromPeer[DT : ClassTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
  }
}
