package scorex.core.network.message


import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.Message._
import scorex.core.serialization.ScorexKryoPool

import scala.util.Try


object BasicMsgDataTypes {

  case class ModifiersData(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])

  case class InvData(typeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class PeersData(peers: Seq[InetSocketAddress])

}

import scorex.core.network.message.BasicMsgDataTypes._

class SyncInfoSpec[SI <: SyncInfo](val serializer: ScorexKryoPool, val c: Class[SI])
  extends MessageSpec[SI] {

  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"
}

object InvSpec extends MessageSpec[InvData] {
  override val c: Class[InvData] = classOf[InvData]

  override val messageCode: MessageCode = 55: Byte
  override val messageName: String = "Inv"
}


object RequestModifierSpec extends MessageSpec[InvData] {
  override val c: Class[InvData] = classOf[InvData]

  override val messageCode: MessageCode = 22: Byte
  override val messageName: String = "RequestModifier"
}


object ModifiersSpec extends MessageSpec[ModifiersData] {
  override val c: Class[ModifiersData] = classOf[ModifiersData]
  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"
}

object GetPeersSpec extends MessageSpec[Unit] {

  override val c: Class[Unit] = classOf[Unit]
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"
}

object PeersSpec extends MessageSpec[PeersData] {
  override val c: Class[PeersData] = classOf[PeersData]

  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

}