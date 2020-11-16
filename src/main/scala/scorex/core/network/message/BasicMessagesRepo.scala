package scorex.core.network.message


import scorex.core.consensus.SyncInfo
import scorex.core.network._
import scorex.core.network.message.Message.MessageCode
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.collection.immutable

case class ModifiersData(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])

case class InvData(typeId: ModifierTypeId, ids: Seq[ModifierId])

/**
  * The `SyncInfo` message requests an `Inv` message that provides modifier ids
  * required be sender to synchronize his blockchain with the recipient.
  * It allows a peer which has been disconnected or started for the first
  * time to get the data it needs to request the blocks it hasn't seen.
  *
  * Payload of this message should be determined in underlying applications.
  */
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

/**
  * The `Inv` message (inventory message) transmits one or more inventories of
  * objects known to the transmitting peer.
  * It can be sent unsolicited to announce new transactions or blocks,
  * or it can be sent in reply to a `SyncInfo` message (or application-specific messages like `GetMempool`).
  *
  */
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

/**
  * The `RequestModifier` message requests one or more modifiers from another node.
  * The objects are requested by an inventory, which the requesting node
  * typically received previously by way of an `Inv` message.
  *
  * This message cannot be used to request arbitrary data, such as historic transactions no
  * longer in the memory pool. Full nodes may not even be able to provide older blocks if
  * they’ve pruned old transactions from their block database.
  * For this reason, the `RequestModifier` message should usually only be used to request
  * data from a node which previously advertised it had that data by sending an `Inv` message.
  *
  */
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

/**
  * The `Modifier` message is a reply to a `RequestModifier` message which requested these modifiers.
  */
class ModifiersSpec(maxMessageSize: Int) extends MessageSpecV1[ModifiersData] with ScorexLogging {

  import ModifiersSpec._

  override val messageCode: MessageCode = MessageCode
  override val messageName: String = MessageName

  private val HeaderLength = 5 // msg type Id + modifiersCount

  override def serialize(data: ModifiersData, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = modifiers.foldLeft((0, HeaderLength)) { case ((c, s), (id, modifier)) =>
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
      log.warn(s"Message with modifiers ${modifiers.keySet} has size $msgSize exceeding limit $maxMessageSize." +
        s" Sending ${w.length() - start} bytes instead")
    }
  }

  override def parse(r: Reader): ModifiersData = {
    val typeId = ModifierTypeId @@ r.getByte() // 1 byte
    val count = r.getUInt().toIntExact // 8 bytes
    val resMap = immutable.Map.newBuilder[ModifierId, Array[Byte]]
    (0 until count).foldLeft(HeaderLength) { case (msgSize, _) =>
      val id = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
      val objBytesCnt = r.getUInt().toIntExact
      val newMsgSize = msgSize + NodeViewModifier.ModifierIdSize + objBytesCnt
      if (newMsgSize > maxMessageSize) {
        throw new Exception("Too big message with modifiers, size: " + maxMessageSize)
      }
      val obj = r.getBytes(objBytesCnt)
      resMap += (id -> obj)
      newMsgSize
    }
    ModifiersData(typeId, resMap.result())
  }
}

/**
  * The `GetPeer` message requests an `Peers` message from the receiving node,
  * preferably one with lots of `PeerSpec` of other receiving nodes.
  * The transmitting node can use those `PeerSpec` addresses to quickly update
  * its database of available nodes rather than waiting for unsolicited `Peers`
  * messages to arrive over time.
  */
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

  val MaxPeersInMessage: Int = 100

  val messageCode: Message.MessageCode = 2: Byte

  val messageName: String = "Peers message"

}

/**
  * The `Peers` message is a reply to a `GetPeer` message and relays connection information about peers
  * on the network.
  */
class PeersSpec(featureSerializers: PeerFeature.Serializers, peersLimit: Int) extends MessageSpecV1[Seq[PeerSpec]] {
  private val peerSpecSerializer = new PeerSpecSerializer(featureSerializers)

  override val messageCode: Message.MessageCode = PeersSpec.messageCode

  override val messageName: String = PeersSpec.messageName

  override def serialize(peers: Seq[PeerSpec], w: Writer): Unit = {
    w.putUInt(peers.size)
    peers.foreach(p => peerSpecSerializer.serialize(p, w))
  }

  override def parse(r: Reader): Seq[PeerSpec] = {
    val length = r.getUInt().toIntExact
    require(length <= peersLimit, s"Too many peers. $length exceeds limit $peersLimit")
    (0 until length).map { _ =>
      peerSpecSerializer.parse(r)
    }
  }
}

object HandshakeSpec {

  val messageCode: MessageCode = 75: Byte
  val messageName: String = "Handshake"
}

/**
  * The `Handshake` message provides information about the transmitting node
  * to the receiving node at the beginning of a connection. Until both peers
  * have exchanged `Handshake` messages, no other messages will be accepted.
  */
class HandshakeSpec(featureSerializers: PeerFeature.Serializers, sizeLimit: Int) extends MessageSpecV1[Handshake] {

  private val peersDataSerializer = new PeerSpecSerializer(featureSerializers)

  override val messageCode: MessageCode = HandshakeSpec.messageCode
  override val messageName: String = HandshakeSpec.messageName

  /**
    * Serializing handshake into a byte writer.
    * @param hs - handshake instance
    * @param w - writer to write bytes to
    */
  override def serialize(hs: Handshake, w: Writer): Unit = {
    // first writes down handshake time, then peer specification of our node
    w.putULong(hs.time)
    peersDataSerializer.serialize(hs.peerSpec, w)
  }

  override def parse(r: Reader): Handshake = {
    require(r.remaining <= sizeLimit, s"Too big handshake. Size ${r.remaining} exceeds $sizeLimit limit")
    val t = r.getULong()
    val data = peersDataSerializer.parse(r)
    Handshake(data, t)
  }
}