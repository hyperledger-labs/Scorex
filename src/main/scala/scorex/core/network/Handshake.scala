package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Ints, Longs, Shorts}
import scorex.core.app.{ApplicationVersionSerializer, Version}
import scorex.core.serialization.Serializer
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

class HandshakeSerializer(featureSerializers: Map[PeerFeature.Id, Serializer[_ <: PeerFeature]],
                          maxHandshakeSize: Int) extends Serializer[Handshake] {

  override def toBytes(obj: Handshake): Array[Byte] = {
    val anb = obj.applicationName.getBytes

    val fab = obj.declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = obj.nodeName.getBytes

    val featureBytes = obj.features.foldLeft(Array(obj.features.size.toByte)){case (fb, f) =>
      val featId = f.featureId
      val featBytes = f.bytes
      fb ++ (featId +: Shorts.toByteArray(featBytes.length.toShort)) ++ featBytes
    }

    Array(anb.size.toByte) ++
      anb ++
      obj.protocolVersion.bytes ++
      Array(nodeNameBytes.size.toByte) ++
      nodeNameBytes ++
      Ints.toByteArray(fab.length) ++
      fab ++
      featureBytes ++
      Longs.toByteArray(obj.time)
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
    position +=1

    val feats = (1 to featuresCount).flatMap{_ =>
      val featId = bytes.slice(position, position + 1).head
      position += 1

      val featBytesCount = Shorts.fromByteArray(bytes.slice(position, position + 2))
      position += 2

      //we ignore a feature found in the handshake if we do not know how to parse it or failed to do that

      val featOpt = featureSerializers.get(featId).flatMap{featureSerializer =>
        featureSerializer.parseBytes(bytes.slice(position, position + featBytesCount)).toOption
      }
      position += featBytesCount

      featOpt
    }

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, isaOpt, feats, time)
  }
}