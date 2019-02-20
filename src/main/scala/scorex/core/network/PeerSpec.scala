package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.app.{ApplicationVersionSerializer, Version}
import scorex.core.network.peer.LocalAddressPeerFeature
import scorex.core.serialization.ScorexSerializer
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}

/**
  * Declared information about peer
  *
  * @param agentName       - Network agent name. May contain information about client code
  *                        stack, starting from core code-base up to the end graphical interface.
  *                        Basic format is `/Name:Version(comments)/Name:Version/.../`,
  *                        e.g. `/Ergo-Scala-client:2.0.0(iPad; U; CPU OS 3_2_1)/AndroidBuild:0.8/`
  * @param protocolVersion - Identifies protocol version being used by the node
  * @param nodeName        - Custom node name
  * @param declaredAddress - Public network address of the node if any
  * @param features        - Set of node capabilities
  */
case class PeerSpec(agentName: String,
                    protocolVersion: Version,
                    nodeName: String,
                    declaredAddress: Option[InetSocketAddress],
                    features: Seq[PeerFeature]) {

  lazy val localAddressOpt: Option[InetSocketAddress] = {
    features.collectFirst { case LocalAddressPeerFeature(addr) => addr }
  }

  def reachablePeer: Boolean = address.isDefined

  def address: Option[InetSocketAddress] = declaredAddress orElse localAddressOpt

}

class PeerSpecSerializer(featureSerializers: PeerFeature.Serializers) extends ScorexSerializer[PeerSpec] {
  override def serialize(obj: PeerSpec, w: Writer): Unit = {

    w.putShortString(obj.agentName)
    ApplicationVersionSerializer.serialize(obj.protocolVersion, w)
    w.putShortString(obj.nodeName)


    w.putOption(obj.declaredAddress) { (writer, isa) =>
      val addr = isa.getAddress.getAddress
      writer.put((addr.size + 4).toByteExact)
      writer.putBytes(addr)
      writer.putUInt(isa.getPort)
    }

    w.put(obj.features.size.toByteExact)
    obj.features.foreach { f =>
      w.put(f.featureId)
      val fBytes = f.bytes
      w.putUShort(fBytes.length.toShortExact)
      w.putBytes(fBytes)
    }
  }

  override def parse(r: Reader): PeerSpec = {

    val appName = r.getShortString()
    require(appName.nonEmpty)

    val protocolVersion = ApplicationVersionSerializer.parse(r)

    val nodeName = r.getShortString()

    val declaredAddressOpt = r.getOption {
      val fas = r.getUByte()
      val fa = r.getBytes(fas - 4)
      val port = r.getUInt().toIntExact
      new InetSocketAddress(InetAddress.getByAddress(fa), port)
    }

    val featuresCount = r.getByte()
    val feats = (1 to featuresCount).flatMap { _ =>
      val featId = r.getByte()
      val featBytesCount = r.getUShort().toShortExact
      val featChunk = r.getChunk(featBytesCount)
      //we ignore a feature found in the PeersData if we do not know how to parse it or failed to do that
      featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseTry(r.newReader(featChunk)).toOption
      }
    }

    PeerSpec(appName, protocolVersion, nodeName, declaredAddressOpt, feats)
  }

}
