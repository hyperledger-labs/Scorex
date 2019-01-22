package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.app.{ApplicationVersionSerializer, Version}
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization._
import scorex.util.Extensions._

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


    w.putOption(obj.declaredAddress) { (writer, isa) =>
      val addr = isa.getAddress.getAddress
      writer.putInt(addr.size + 4)
      writer.putBytes(addr)
      writer.putInt(isa.getPort)
    }

    w.put(obj.features.size.toByteExact)
    obj.features.foreach { f =>
      w.put(f.featureId)
      val fwriter = w.newWriter()
      f.serializer.serialize(f, fwriter)
      w.putShort(fwriter.length.toShortExact)
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

    val declaredAddressOpt = r.getOption {
      val fas = r.getInt()
      val fa = r.getBytes(fas - 4)
      val port = r.getInt()
      new InetSocketAddress(InetAddress.getByAddress(fa), port)
    }

    val featuresCount = r.getByte()
    val feats = (1 to featuresCount).flatMap { _ =>
      val featId = r.getByte()
      val featBytesCount = r.getShort()
      val featChunk = r.getChunk(featBytesCount)
      //we ignore a feature found in the handshake if we do not know how to parse it or failed to do that
      featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseTry(r.newReader(featChunk)).toOption
      }
    }

    val time = r.getLong()
    Handshake(appName, protocolVersion, nodeName, declaredAddressOpt, feats, time)
  }
}