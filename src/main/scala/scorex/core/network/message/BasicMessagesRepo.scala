package scorex.core.network.message


import java.net.{InetAddress, InetSocketAddress}

import akka.util.ByteString
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.Message.MessageCode
import scorex.core.newserialization._
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

object BasicMsgDataTypes {
  type InvData = (ModifierTypeId, Seq[ModifierId])
//  type ModifiersData = (ModifierTypeId, Map[ModifierId, Array[Byte]])
  case class ModifiersData(typeId: ModifierTypeId,
                           modifiers: Map[ModifierId, ByteString])
}

import scorex.core.network.message.BasicMsgDataTypes._

class SyncInfoMessageSpec[SI <: SyncInfo](serializer: ScorexSerializer[SI]) extends MessageSpec[SI] {

  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"


  override def serialize(data: SI, w: ScorexWriter): Unit = {
    serializer.serialize(data, w)
  }

  override def parse(r: ScorexReader): SI = {
    serializer.parse(r)
  }
}

object InvSpec {
  val MessageCode: Byte = 55
  val MessageName: String = "Inv"
}

class InvSpec(maxInvObjects: Int) extends MessageSpec[InvData] {

  import InvSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  override def serialize(data: InvData, w: ScorexWriter): Unit = {
    val (typeId, elems) = data
    require(elems.nonEmpty, "empty inv list")
    require(elems.lengthCompare(maxInvObjects) <= 0, s"more invs than $maxInvObjects in a message")
    w.put(typeId)
    w.putInt(elems.size)
    elems.foreach { id =>
      val bytes = idToBytes(id)
      assert(bytes.length == NodeViewModifier.ModifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: ScorexReader): InvData = {
    val typeId = ModifierTypeId @@ r.getByte()
    val count = r.getInt()
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map { c =>
      bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    }

    typeId -> elems
  }
}

object RequestModifierSpec {
  val MessageCode: MessageCode = 22: Byte
  val MessageName: String = "RequestModifier"
}

class RequestModifierSpec(maxInvObjects: Int) extends MessageSpec[InvData] {

  import RequestModifierSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  private val invSpec = new InvSpec(maxInvObjects)


  override def serialize(data: InvData, w: ScorexWriter): Unit = {
    invSpec.serialize(data, w)
  }

  override def parse(r: ScorexReader): InvData = {
    invSpec.parse(r)
  }
}

object ModifiersSpec {
  val MessageCode: MessageCode = 33: Byte
  val MessageName: String = "Modifier"
}

class ModifiersSpec(maxMessageSize: Int) extends MessageSpec[ModifiersData] with ScorexLogging {

  import ModifiersSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  override def serialize(data: ModifiersData, w: ScorexWriter): Unit = {

    require(data.modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = data.modifiers.foldLeft((0, 5)) { case ((c, s), (id, modifier)) =>
      val size = s + NodeViewModifier.ModifierIdSize + 4 + modifier.length
      val count = if (size <= maxMessageSize) c + 1 else c
      count -> size
    }

    val start = w.length()
    w.put(data.typeId)
    w.putInt(msgCount)

    data.modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(idToBytes(id))
      w.putInt(modifier.length)
      w.putByteString2(modifier)
    }

    if (msgSize > maxMessageSize) {
      log.warn(s"Message with modifiers ${data.modifiers.keySet} have size $msgSize exceeding limit $maxMessageSize." +
        s" Sending ${w.length() - start} bytes instead")
    }
  }

  override def parse(r: ScorexReader): ModifiersData = {
    val typeId = ModifierTypeId @@ r.getByte()
    val count = r.getInt()
    val seq = (0 until count).map { _ =>
      val id = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getInt()
      val obj = r.getByteString2(objBytesCnt)
      id -> obj
    }
    ModifiersData(typeId, seq.toMap)
  }
}

object GetPeersSpec extends MessageSpec[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def serialize(obj: Unit, w: ScorexWriter): Unit = {
  }

  override def parse(r: ScorexReader): Unit = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
  }
}

object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  override val messageCode: Message.MessageCode = 2: Byte

  override val messageName: String = "Peers message"

  override def serialize(peers: Seq[InetSocketAddress], w: ScorexWriter): Unit = {
    w.putInt(peers.size)
    peers.foreach { peer =>
      w.putBytes(peer.getAddress.getAddress)
      w.putInt(peer.getPort)
    }
  }

  override def parse(r: ScorexReader): Seq[InetSocketAddress] = {
    val length = r.getInt()
    require(r.remaining == (length * (AddressLength + PortLength)), "Data does not match length")
    (0 until length).map { _ =>
      val address = InetAddress.getByAddress(r.getBytes(AddressLength))
      val port = r.getInt()
      new InetSocketAddress(address, port)
    }
  }
}
