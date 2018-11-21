package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.app.{ApplicationVersionSerializer, Version}
import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer

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


class HandshakeSerializer(featureSerializers: PeerFeature.Serializers,
                             maxHandshakeSize: Int) extends ScorexSerializer[Handshake] {

  override def serialize(obj: Handshake, w: Writer): Unit = {

    w.putShortString(obj.applicationName)
    ApplicationVersionSerializer.serialize(obj.protocolVersion, w)
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
      val fwriter = w.newWriter()
      f.serializer.serialize(f, fwriter)
      w.putShort(fwriter.length.toShort)
      w.append(fwriter)
    }
    w.putLong(obj.time)
  }

  override def parse(r: Reader): Handshake = {

    require(r.remaining <= maxHandshakeSize)

    val appName = r.getShortString()
    require(appName.size > 0)

    val protocolVersion = ApplicationVersionSerializer.parse(r)

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
      val featChunk = r.getChunk(featBytesCount)
      //we ignore a feature found in the handshake if we do not know how to parse it or failed to do that
      val featOpt = featureSerializers.get(featId).flatMap { featureSerializer =>
        Try {
          featureSerializer.parse(r.newReader(featChunk))
        }.toOption
      }
      featOpt
    }

    val time = r.getLong()
    Handshake(appName, protocolVersion, nodeName, declaredAddressOpt, feats, time)
  }
}