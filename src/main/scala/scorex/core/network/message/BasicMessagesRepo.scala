package scorex.core.network.message


import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.Message.{MessageCode, _}
import scorex.core.settings.NetworkSettings

import scala.util.Try


object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
  type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
}

import scorex.core.network.message.BasicMsgDataTypes._

class SyncInfoMessageSpec[SI <: SyncInfo](deserializer: Array[Byte] => Try[SI]) extends MessageSpec[SI] {

  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"

  override def parseBytes(bytes: Array[Byte]): Try[SI] = deserializer(bytes)

  override def toBytes(data: SI): Array[Byte] = data.bytes
}

object InvSpec {
  val MessageCode = 55: Byte
  val MessageName: String = "Inv"
}
class InvSpec(maxInvObjects: Int) extends MessageSpec[InvData] {
  import InvSpec._

  override val messageCode = MessageCode
  override val messageName: String = MessageName

  override def parseBytes(bytes: Array[Byte]): Try[InvData] = Try {
    val typeId = ModifierTypeId @@ bytes.head
    val count = Ints.fromByteArray(bytes.slice(1, 5)) // scalastyle:ignore magic.number

    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"more invs than $maxInvObjects in a message")

    val elems = (0 until count).map { c =>
      ModifierId @@ bytes.slice(5 + c * NodeViewModifier.ModifierIdSize, 5 + (c + 1) * NodeViewModifier.ModifierIdSize)
    }

    typeId -> elems
  }

  override def toBytes(data: InvData): Array[Byte] = {
    require(data._2.nonEmpty, "empty inv list")
    require(data._2.size <= maxInvObjects, s"more invs than $maxInvObjects in a message")
    data._2.foreach(e => require(e.length == NodeViewModifier.ModifierIdSize))

    Bytes.concat(Array(data._1), Ints.toByteArray(data._2.size), scorex.core.utils.concatBytes(data._2))
  }
}

object RequestModifierSpec {
  val MessageCode: MessageCode = 22: Byte
  val MessageName: String = "RequestModifier"
}
class RequestModifierSpec(maxInvObjects: Int)
  extends MessageSpec[InvData] {
  import RequestModifierSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  private val invSpec = new InvSpec(maxInvObjects)

  override def toBytes(typeAndId: InvData): Array[Byte] =
    invSpec.toBytes(typeAndId)

  override def parseBytes(bytes: Array[Byte]): Try[InvData] =
    invSpec.parseBytes(bytes)
}


object ModifiersSpec extends MessageSpec[ModifiersData] {

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  override def parseBytes(bytes: Array[Byte]): Try[ModifiersData] = Try {
    val typeId = ModifierTypeId @@ bytes.head
    val count = Ints.fromByteArray(bytes.slice(1, 5)) // scalastyle:ignore magic.number
    val objBytes = bytes.slice(5, bytes.length)       // scalastyle:ignore magic.number
    val (_, seq) = (0 until count).foldLeft(0 -> Seq[(ModifierId, Array[Byte])]()) {
      case ((pos, collected), _) =>

        val id = ModifierId @@ objBytes.slice(pos, pos + NodeViewModifier.ModifierIdSize)
        val objBytesCnt = Ints.fromByteArray(objBytes.slice(pos + NodeViewModifier.ModifierIdSize, pos + NodeViewModifier.ModifierIdSize + 4))
        val obj = objBytes.slice(pos + NodeViewModifier.ModifierIdSize + 4, pos + NodeViewModifier.ModifierIdSize + 4 + objBytesCnt)

        (pos + NodeViewModifier.ModifierIdSize + 4 + objBytesCnt) -> (collected :+ (id -> obj))
    }
    typeId -> seq.toMap
  }

  override def toBytes(data: ModifiersData): Array[Byte] = {
    require(data._2.nonEmpty, "empty modifiers list")
    val typeId = data._1
    val modifiers = data._2
    Array(typeId) ++ Ints.toByteArray(modifiers.size) ++ modifiers.map { case (id, modifier) =>
      id ++ Ints.toByteArray(modifier.length) ++ modifier
    }.reduce(_ ++ _)
  }
}

object GetPeersSpec extends MessageSpec[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def parseBytes(bytes: Array[Byte]): Try[Unit] =
    Try(require(bytes.isEmpty, "Non-empty data for GetPeers"))

  override def toBytes(data: Unit): Array[Byte] = Array()
}

object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

  override def parseBytes(bytes: Array[Byte]): Try[Seq[InetSocketAddress]] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    require(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    (0 until length).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    }
  }

  override def toBytes(peers: Seq[InetSocketAddress]): Array[Byte] = {
    val length = peers.size
    val lengthBytes = Ints.toByteArray(length)

    peers.foldLeft(lengthBytes) { case (bs, peer) =>
      Bytes.concat(bs, peer.getAddress.getAddress, Ints.toByteArray(peer.getPort))
    }
  }
}
