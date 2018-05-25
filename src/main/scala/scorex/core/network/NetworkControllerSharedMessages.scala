package scorex.core.network

import scorex.core.network.message.MessageSpec

import scala.reflect.runtime.universe.TypeTag

// Messages shared by NetworkController, PeerSynchronizer and NodeViewSynchronizer
object NetworkControllerSharedMessages {
  object ReceivableMessages {
    case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
  }
}
