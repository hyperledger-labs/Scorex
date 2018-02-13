package scorex.core.network

import scorex.core.network.message.MessageSpec
import scala.reflect.runtime.universe.TypeTag

trait NetworkControllerSharedMessages {
  case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
}
