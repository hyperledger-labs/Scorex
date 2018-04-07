package scorex.core.network.message


import java.net.{InetAddress, InetSocketAddress}
import java.util

import akka.util.ByteString
import com.google.common.primitives.{Bytes, Ints}
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.Message.{MessageCode, _}

import scala.util.Try


object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
  type ModifiersData = (ModifierTypeId, Map[ModifierId, Seq[Byte]])
}

import scorex.core.network.message.BasicMsgDataTypes._

class SyncInfoMessageSpec[SI <: SyncInfo](deserializer: Seq[Byte] => Try[SI]) extends MessageSpec[SI] {

  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"

  override def parseBytes(bytes: Seq[Byte]): Try[SI] = deserializer(bytes)

  override def toBytes(data: SI): Seq[Byte] = data.bytes
}

object InvSpec {
  val MessageCode = 55: Byte
  val MessageName: String = "Inv"
}
class InvSpec(maxInvObjects: Int) extends MessageSpec[InvData] {
  import InvSpec._

  override val messageCode = MessageCode
  override val messageName: String = MessageName

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def parseBytes(bytes: Seq[Byte]): Try[InvData] = Try {
    val typeId = ModifierTypeId @@ bytes.head
    val count = Ints.fromByteArray(bytes.slice(1, 5).toArray)

    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"more invs than $maxInvObjects in a message")

    val elems = (0 until count).map { c =>
      ModifierId @@ bytes.slice(5 + c * NodeViewModifier.ModifierIdSize, 5 + (c + 1) * NodeViewModifier.ModifierIdSize)
    }

    typeId -> elems
  }

  override def toBytes(data: InvData): Seq[Byte] = {
    require(data._2.nonEmpty, "empty inv list")
    require(data._2.size <= maxInvObjects, s"more invs than $maxInvObjects in a message")
    data._2.foreach(e => require(e.length == NodeViewModifier.ModifierIdSize))

    ByteString(data._1) ++ Ints.toByteArray(data._2.size) ++ data._2.flatten
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

  override def toBytes(typeAndId: InvData): Seq[Byte] =
    invSpec.toBytes(typeAndId)

  override def parseBytes(bytes: Seq[Byte]): Try[InvData] =
    invSpec.parseBytes(bytes)
}


object ModifiersSpec extends MessageSpec[ModifiersData] {

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def parseBytes(bytes: Seq[Byte]): Try[ModifiersData] = Try {
    val typeId = ModifierTypeId @@ bytes.head
    val count = Ints.fromByteArray(bytes.slice(1, 5).toArray)
    val objBytes = bytes.slice(5, bytes.length)
    val (_, seq) = (0 until count).foldLeft(0 -> Seq[(ModifierId, Seq[Byte])]()) {
      case ((pos, collected), _) =>

        val id = ModifierId @@ objBytes.slice(pos, pos + NodeViewModifier.ModifierIdSize)
        val objBytesCnt = Ints.fromByteArray(objBytes.slice(pos + NodeViewModifier.ModifierIdSize, pos + NodeViewModifier.ModifierIdSize + 4).toArray)
        val obj = objBytes.slice(pos + NodeViewModifier.ModifierIdSize + 4, pos + NodeViewModifier.ModifierIdSize + 4 + objBytesCnt)

        (pos + NodeViewModifier.ModifierIdSize + 4 + objBytesCnt) -> (collected :+ (id -> obj))
    }
    typeId -> seq.toMap
  }

  override def toBytes(data: ModifiersData): Seq[Byte] = {
    require(data._2.nonEmpty, "empty modifiers list")
    val typeId = data._1
    val modifiers = data._2
    ByteString(typeId) ++ Ints.toByteArray(modifiers.size) ++ modifiers.map { case (id, modifier) =>
      id ++ Ints.toByteArray(modifier.length) ++ modifier
    }.fold(ByteString())(_ ++ _)
  }
}

object GetPeersSpec extends MessageSpec[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def parseBytes(bytes: Seq[Byte]): Try[Unit] =
    Try(require(bytes.isEmpty, "Non-empty data for GetPeers"))

  override def toBytes(data: Unit): Seq[Byte] = ByteString()
}

object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

  override def parseBytes(bytes: Seq[Byte]): Try[Seq[InetSocketAddress]] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes.toArray, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    require(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    (0 until length).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes.toArray, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(bytes.toArray, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    }
  }

  override def toBytes(peers: Seq[InetSocketAddress]): Seq[Byte] = {
    val length = peers.size
    val lengthBytes = Ints.toByteArray(length)

    peers.foldLeft(ByteString(lengthBytes)) { (bs, peer) =>
      bs ++ peer.getAddress.getAddress ++ Ints.toByteArray(peer.getPort)
    }
  }
}