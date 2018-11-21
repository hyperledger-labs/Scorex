package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: Id = LocalAddressPeerFeature.featureId

  override def serializer: LocalAddressPeerFeatureSerializer.type = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeature {
  val featureId: Id = 2: Byte
}

object LocalAddressPeerFeatureSerializer extends ScorexSerializer[LocalAddressPeerFeature] {

  override def serialize(obj: LocalAddressPeerFeature, w: Writer): Unit = {
    w.putBytes(obj.address.getAddress.getAddress)
    w.putInt(obj.address.getPort)
  }

  override def parse(r: Reader): LocalAddressPeerFeature = {
    val fa = r.getBytes(4)
    val port = r.getInt()
    LocalAddressPeerFeature(new InetSocketAddress(InetAddress.getByAddress(fa), port))
  }
}