package scorex.core.network.peer

import java.net.{InetAddress, InetSocketAddress}

import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.util.serialization._
import scorex.core.serialization.ScorexSerializer
import scorex.util.Extensions._

case class SessionIdPeerFeature(networkMagic:Array[Byte], sessionId:Long = scala.util.Random.nextLong()) extends PeerFeature {
  override type M = SessionIdPeerFeature
  override val featureId: Id = SessionIdPeerFeature.featureId

  override def serializer: SessionIdPeerFeatureSerializer.type = SessionIdPeerFeatureSerializer
}

object SessionIdPeerFeature {
  val featureId: Id = 3: Byte
}

object SessionIdPeerFeatureSerializer extends ScorexSerializer[SessionIdPeerFeature] {
  override def serialize(obj: SessionIdPeerFeature, w: Writer): Unit = {
    w.putInt(obj.networkMagic.size)
    w.putBytes(obj.networkMagic)
    w.putLong(obj.sessionId)
  }

  override def parse(r: Reader): SessionIdPeerFeature = {
    val networkMagic = r.getBytes(r.getInt())
    val sessionId = r.getLong()
    SessionIdPeerFeature(networkMagic, sessionId)
  }
}
