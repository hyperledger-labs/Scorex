package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Ints, Longs}
import scorex.core.app.{ApplicationVersion, ApplicationVersionSerializer}
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try


case class Handshake(applicationName: String,
                     applicationVersion: ApplicationVersion,
                     nodeName: String,
                     nodeNonce: Long,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long) extends BytesSerializable {

  require(Option(applicationName).isDefined)
  require(Option(applicationVersion).isDefined)

  override type M = Handshake

  override def serializer: Serializer[Handshake] = HandshakeSerializer
}

object HandshakeSerializer extends Serializer[Handshake] {

  override def bytes(obj: Handshake): Array[Byte] = {
    val anb = obj.applicationName.getBytes

    val fab = obj.declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = obj.nodeName.getBytes

    Array(anb.size.toByte) ++ anb ++
      obj.applicationVersion.bytes ++
      Array(nodeNameBytes.size.toByte) ++ nodeNameBytes ++
      Longs.toByteArray(obj.nodeNonce) ++
      Ints.toByteArray(fab.length) ++ fab ++
      Longs.toByteArray(obj.time)

  }

  override def parseBytes(bytes: Array[Byte]): Try[Handshake] = Try {
    var position = 0
    val appNameSize = bytes.head
    require(appNameSize > 0)

    position += 1

    val an = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av = ApplicationVersionSerializer.parseBytes(
      bytes.slice(position, position + ApplicationVersionSerializer.SerializedVersionLength)).get
    position += ApplicationVersionSerializer.SerializedVersionLength

    val nodeNameSize = bytes.slice(position, position + 1).head
    position += 1

    val nodeName = new String(bytes.slice(position, position + nodeNameSize))
    position += nodeNameSize

    val nonce = Longs.fromByteArray(bytes.slice(position, position + 8))
    position += 8

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if (fas > 0) {
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, nonce, isaOpt, time)
  }
}