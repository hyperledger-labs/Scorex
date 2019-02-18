package scorex.core.network.message


import scorex.core.consensus.SyncInfo
import scorex.core.network._
import scorex.core.network.message.Message.MessageCode
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

case class ModifiersData(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])

case class InvData(typeId: ModifierTypeId, ids: Seq[ModifierId])

class SyncInfoMessageSpec[SI <: SyncInfo](serializer: ScorexSerializer[SI]) extends MessageSpecV1[SI] {


  override val messageCode: MessageCode = 65: Byte
  override val messageName: String = "Sync"

  override def serialize(data: SI, w: Writer): Unit = serializer.serialize(data, w)

  override def parse(r: Reader): SI = serializer.parse(r)
}

object InvSpec {
  val MessageCode: Byte = 55
  val MessageName: String = "Inv"
}

class InvSpec(maxInvObjects: Int) extends MessageSpecV1[InvData] {

  import InvSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  override def serialize(data: InvData, w: Writer): Unit = {
    val typeId = data.typeId
    val elems = data.ids
    require(elems.nonEmpty, "empty inv list")
    require(elems.lengthCompare(maxInvObjects) <= 0, s"more invs than $maxInvObjects in a message")
    w.put(typeId)
    w.putUInt(elems.size)
    elems.foreach { id =>
      val bytes = idToBytes(id)
      assert(bytes.length == NodeViewModifier.ModifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: Reader): InvData = {
    val typeId = ModifierTypeId @@ r.getByte()
    val count = r.getUInt().toIntExact
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map { c =>
      bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    }

    InvData(typeId, elems)
  }

}

object RequestModifierSpec {
  val MessageCode: MessageCode = 22: Byte
  val MessageName: String = "RequestModifier"
}

class RequestModifierSpec(maxInvObjects: Int) extends MessageSpecV1[InvData] {

  import RequestModifierSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  private val invSpec = new InvSpec(maxInvObjects)


  override def serialize(data: InvData, w: Writer): Unit = {
    invSpec.serialize(data, w)
  }

  override def parse(r: Reader): InvData = {
    invSpec.parse(r)
  }
}

object ModifiersSpec {
  val MessageCode: MessageCode = 33: Byte
  val MessageName: String = "Modifier"
}

class ModifiersSpec(maxMessageSize: Int) extends MessageSpecV1[ModifiersData] with ScorexLogging {

  import ModifiersSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName


  override def serialize(data: ModifiersData, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = modifiers.foldLeft((0, 5)) { case ((c, s), (id, modifier)) =>
      val size = s + NodeViewModifier.ModifierIdSize + 4 + modifier.length
      val count = if (size <= maxMessageSize) c + 1 else c
      count -> size
    }

    val start = w.length()
    w.put(typeId)
    w.putUInt(msgCount)

    modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(idToBytes(id))
      w.putUInt(modifier.length)
      w.putBytes(modifier)
    }

    if (msgSize > maxMessageSize) {
      log.warn(s"Message with modifiers ${modifiers.keySet} have size $msgSize exceeding limit $maxMessageSize." +
        s" Sending ${w.length() - start} bytes instead")
    }
  }

  override def parse(r: Reader): ModifiersData = {
    val typeId = ModifierTypeId @@ r.getByte()
    val count = r.getUInt().toIntExact
    val seq = (0 until count).map { _ =>
      val id = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getUInt().toIntExact
      val obj = r.getBytes(objBytesCnt)
      id -> obj
    }
    ModifiersData(typeId, seq.toMap)
  }
}

object GetPeersSpec extends MessageSpecV1[Unit] {
  override val messageCode: Message.MessageCode = 1: Byte

  override val messageName: String = "GetPeers message"

  override def serialize(obj: Unit, w: Writer): Unit = {
  }

  override def parse(r: Reader): Unit = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
  }
}

object PeersSpec {

  /**
    * Limit number of peers to send to always fit in 0.5 Mb.
    */
  val MaxPeersInMessage: Int = 524288 / (HandshakeSpec.MaxHandshakeSize + 4)

  val messageCode: Message.MessageCode = 2: Byte

  val messageName: String = "Peers message"

}

class PeersSpec(featureSerializers: PeerFeature.Serializers) extends MessageSpecV1[Seq[PeerData]] {
  private val peerDataSerializer = new PeerDataSerializer(featureSerializers)

  override val messageCode: Message.MessageCode = PeersSpec.messageCode

  override val messageName: String = PeersSpec.messageName

  override def serialize(peers: Seq[PeerData], w: Writer): Unit = {
    w.putUInt(peers.size)
    peers.foreach(p => peerDataSerializer.serialize(p, w))

  }

  override def parse(r: Reader): Seq[PeerData] = {
    val length = r.getUInt().toIntExact
    (0 until length).map { _ =>
      peerDataSerializer.parse(r)
    }
  }
}


object HandshakeSpec {
  // todo what is the real limit?
  val MaxHandshakeSize: Int = 16384

  val messageCode: MessageCode = 75: Byte
  val messageName: String = "Handshake"
}

class HandshakeSpec(featureSerializers: PeerFeature.Serializers) extends MessageSpecV1[Handshake] {

  private val peersDataSerializer = new PeerDataSerializer(featureSerializers)

  override val messageCode: MessageCode = HandshakeSpec.messageCode
  override val messageName: String = HandshakeSpec.messageName

  override def serialize(obj: Handshake, w: Writer): Unit = {
    w.putULong(obj.time)
    peersDataSerializer.serialize(obj.peerData, w)
  }

  override def parse(r: Reader): Handshake = {
    val t = r.getULong()
    val data = peersDataSerializer.parse(r)
    Handshake(data, t)
  }
}