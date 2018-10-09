package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import scorex.core.app.{ApplicationVersionSerializer, Version}
import scorex.core.newserialization._

import scala.util.Try

case class Handshake(applicationName: String,
                     protocolVersion: Version,
                     nodeName: String,
                     declaredAddress: Option[InetSocketAddress],
                     features: Seq[PeerFeature],
                     time: Long) {

  require(Option(applicationName).isDefined)
  require(Option(protocolVersion).isDefined)
}


class NewHandshakeSerializer(featureSerializers: PeerFeature.Serializers,
                             maxHandshakeSize: Int) extends scorex.core.newserialization.ScorexSerializer[Handshake] {

  override def parse(r: ScorexReader): Handshake = {

    require(r.remaining <= maxHandshakeSize)

    val appName = r.getShortString()
    require(appName.size > 0)

    val protocolVersion = ApplicationVersionSerializer.parseBytes(
      r.getBytes(ApplicationVersionSerializer.SerializedVersionLength)
    ).get

    val nodeName = r.getShortString()

    val fas = r.getInt()
    val declaredAddressOpt = if (fas > 0) {
      val fa = r.getBytes(fas - 4)
      val port = r.getInt()
      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val featuresCount = r.getByte()
    val feats = (1 to featuresCount).flatMap { _ =>
      val featId = r.getByte()
      val featBytesCount = r.getShort()
      val featBytes = r.getBytes(featBytesCount)
      //we ignore a feature found in the handshake if we do not know how to parse it or failed to do that
      val featOpt = featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseBytes(featBytes).toOption
      }
      featOpt
    }

    val time = r.getLong()
    Handshake(appName, protocolVersion, nodeName, declaredAddressOpt, feats, time)
  }

  override def serialize(obj: Handshake, w: ScorexWriter): Unit = {

    w.putShortString(obj.applicationName)
    w.putBytes(obj.protocolVersion.bytes)
    w.putShortString(obj.nodeName)

    obj.declaredAddress match {
      case Some(isa) =>
        val addr = isa.getAddress.getAddress
        w.putInt(addr.size + 4)
        w.putBytes(addr)
        w.putInt(isa.getPort)
      case None =>
        w.putInt(0)
    }

    w.put(obj.features.size.toByte)
    obj.features.foreach { f =>
      w.put(f.featureId)
      w.putShort(f.bytes.length.toShort)
      w.putBytes(f.bytes)
    }

    w.putLong(obj.time)
  }
}

class HandshakeSerializer(featureSerializers: PeerFeature.Serializers,
                          maxHandshakeSize: Int) extends scorex.core.serialization.Serializer[Handshake] {

  override def toBytes(obj: Handshake): Array[Byte] = {
    val anb = obj.applicationName.getBytes

    val fab = obj.declaredAddress.map { isa =>
      Bytes.concat(isa.getAddress.getAddress, Ints.toByteArray(isa.getPort))
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = obj.nodeName.getBytes

    val featureBytes = obj.features.foldLeft(Array(obj.features.size.toByte)) { case (fb, f) =>
      val featId = f.featureId
      val featBytes = f.bytes
      Bytes.concat(fb, featId +: Shorts.toByteArray(featBytes.length.toShort), featBytes)
    }

    Bytes.concat(
      Array(anb.size.toByte),
      anb,
      obj.protocolVersion.bytes,
      Array(nodeNameBytes.size.toByte),
      nodeNameBytes,
      Ints.toByteArray(fab.length),
      fab,
      featureBytes,
      Longs.toByteArray(obj.time))
  }

  override def parseBytes(bytes: Array[Byte]): Try[Handshake] = Try {
    require(bytes.length <= maxHandshakeSize)

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

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if (fas > 0) {
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val featuresCount = bytes.slice(position, position + 1).head
    position += 1

    val feats = (1 to featuresCount).flatMap { _ =>
      val featId = bytes.slice(position, position + 1).head
      position += 1

      val featBytesCount = Shorts.fromByteArray(bytes.slice(position, position + 2))
      position += 2

      //we ignore a feature found in the handshake if we do not know how to parse it or failed to do that

      val featOpt = featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseBytes(bytes.slice(position, position + featBytesCount)).toOption
      }
      position += featBytesCount

      featOpt
    }

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, isaOpt, feats, time)
  }
}