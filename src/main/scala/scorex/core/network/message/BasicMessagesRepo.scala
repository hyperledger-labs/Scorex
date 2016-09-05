package scorex.core.network.message


import com.google.common.primitives.{Bytes, Ints}
import scorex.core.network.message.Message._
import scorex.core.transaction.NodeStateModifier
import scala.util.Try
import java.net.{InetAddress, InetSocketAddress}
import java.util

import NodeStateModifier.{ModifierTypeId, ModifierId}

object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
}

import BasicMsgDataTypes._

object InvSpec extends MessageSpec[InvData] {
  //todo: check no more ids in inventory message than MaxObject
  val MaxObjects = 500

  override val messageCode: MessageCode = 55: Byte
  override val messageName: String = "Inv message"

  //todo: implement
  override def deserializeData(bytes: Array[MessageCode]): Try[InvData] = ???

  override def serializeData(data: InvData): Array[Byte] = {
    require(data._2.nonEmpty)
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
    ???

  override def deserializeData(bytes: Array[Byte]): Try[InvData] =
    ???
}


class ModifiersSpec[M <: NodeStateModifier]
  extends MessageSpec[(ModifierTypeId, Seq[M])] {

  override val messageCode: MessageCode = 33: Byte
  override val messageName: String = "Modifier"

  //todo: implement
  override def deserializeData(bytes: Array[Byte]): Try[(ModifierTypeId, Seq[M])] = ???

  //todo: implement
  override def serializeData(data: (ModifierTypeId, Seq[M])): Array[Byte] = ???

  /*
  companion: BlockCompanion[P, TX, B]

  override def serializeData(block: B): Array[Byte] = block.bytes

  override def deserializeData(bytes: Array[Byte]): Try[Block[P, TX]] =
    companion.parse(bytes) */
}

/*
trait SignaturesSeqSpec extends MessageSpec[Seq[SigningFunctions.Signature]] {

  private val SignatureLength = Signature25519.SignatureSize
  private val DataLength = 4

  override def deserializeData(bytes: Array[Byte]): Try[Seq[SigningFunctions.Signature]] = Try {
    val lengthBytes = bytes.take(DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    assert(bytes.length == DataLength + (length * SignatureLength), "Data does not match length")

    (0 until length).map { i =>
      val position = DataLength + (i * SignatureLength)
      bytes.slice(position, position + SignatureLength)
    }
  }

  override def serializeData(signatures: Seq[SigningFunctions.Signature]): Array[Byte] = {
    val length = signatures.size
    val lengthBytes = Ints.toByteArray(length)

    //WRITE SIGNATURES
    signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
  }
}*/

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