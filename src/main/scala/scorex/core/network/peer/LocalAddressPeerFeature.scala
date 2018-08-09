package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Bytes, Ints}
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.Serializer

import scala.util.Try

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: Id = LocalAddressPeerFeature.featureId

  override def serializer: Serializer[LocalAddressPeerFeature] = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeature {
  val featureId: Id = 2: Byte
}

object LocalAddressPeerFeatureSerializer extends Serializer[LocalAddressPeerFeature] {

  override def toBytes(obj: LocalAddressPeerFeature): Array[Byte] = {
    Bytes.concat(obj.address.getAddress.getAddress, Ints.toByteArray(obj.address.getPort))
  }

  override def parseBytes(bytes: Array[Byte]): Try[LocalAddressPeerFeature] = Try {
    require(bytes.length == 8)
    val fa = bytes.slice(0, 4)
    val port = Ints.fromByteArray(bytes.slice(4, 8))
    LocalAddressPeerFeature(new InetSocketAddress(InetAddress.getByAddress(fa), port))
  }
}