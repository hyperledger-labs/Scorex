package scorex.core.network.message


import com.google.common.primitives.{Bytes, Ints}
import scorex.core.network.message.Message._

import scala.util.Try
import java.net.{InetAddress, InetSocketAddress}
import java.util

import scorex.core.transaction.NodeViewModifier
import scorex.core.transaction.NodeViewModifier._


object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
  type ModifiersData = (NodeViewModifier.ModifierTypeId, Map[ModifierId, Array[Byte]])
}

import BasicMsgDataTypes._

object InvSpec extends MessageSpec[InvData] {
  //todo: fetch from settings file?
  val MaxObjects = 500

  override val messageCode: MessageCode = 55: Byte
  override val messageName: String = "Inv"

  override def deserializeData(bytes: Array[MessageCode]): Try[InvData] = Try {
    val typeId = bytes.head
    val count = Ints.fromByteArray(bytes.slice(1, 5))

    assert(count > 0, "empty inv list")
    assert(count <=  MaxObjects, s"more invs than $MaxObjects in a message")

    val elems = (0 until count).map { c =>
      bytes.slice(5 + c * NodeViewModifier.ModifierIdSize, 5 + (c + 1) * NodeViewModifier.ModifierIdSize + 1)
    }

    typeId -> elems
  }

  override def serializeData(data: InvData): Array[Byte] = {
    require(data._2.nonEmpty, "empty inv list")
    require(data._2.size <=  MaxObjects, s"more invs than $MaxObjects in a message")

    Array(data._1) ++
      Ints.toByteArray(data._2.size) ++
      data._2.reduce(_ ++ _)
  }
}


object RequestModifierSpec
  extends MessageSpec[InvData] {

  override val messageCode: MessageCode = 22: Byte
  override val messageName: String = "RequestModifier"

  override def serializeData(typeAndId: InvData): Array[Byte] =
    InvSpec.serializeData(typeAndId)

  override def deserializeData(bytes: Array[Byte]): Try[InvData] =
    InvSpec.deserializeData(bytes)
}


object ModifiersSpec extends MessageSpec[ModifiersData] {

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  //todo: implement
  override def deserializeData(bytes: Array[Byte]): Try[ModifiersData] = ???

  //todo: implement
  override def serializeData(data: ModifiersData): Array[Byte] = ???

  /*
  companion: BlockCompanion[P, TX, B]

  override def serializeData(block: B): Array[Byte] = block.bytes

  override def deserializeData(bytes: Array[Byte]): Try[Block[P, TX]] =
    companion.parse(bytes) */
}

object GetPeersSpec extends MessageSpec[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def deserializeData(bytes: Array[Byte]): Try[Unit] =
    Try(require(bytes.isEmpty, "Non-empty data for GetPeers"))

  override def serializeData(data: Unit): Array[Byte] = Array()
}

object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

  override def deserializeData(bytes: Array[Byte]): Try[Seq[InetSocketAddress]] = Try {
    val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

    (0 until length).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    }
  }

  override def serializeData(peers: Seq[InetSocketAddress]): Array[Byte] = {
    val length = peers.size
    val lengthBytes = Ints.toByteArray(length)

    peers.foldLeft(lengthBytes) { case (bs, peer) =>
      Bytes.concat(bs, peer.getAddress.getAddress, Ints.toByteArray(peer.getPort))
    }
  }
}