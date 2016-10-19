package scorex.core.serialization

import java.net.InetSocketAddress

import com.esotericsoftware.kryo.Kryo
import com.twitter.chill.IKryoRegistrar
import scorex.core.app.ApplicationVersion
import scorex.core.network.Handshake


class ScorexRegistrar extends IKryoRegistrar {
  override def apply(k: Kryo): Unit = {
    k.setRegistrationRequired(true)
    k.setReferences(false)
    k.register(classOf[String], new ByteLengthUtf8StringSerializer)
    k.register(classOf[Handshake], new HandshakeSerializer)
    k.register(classOf[ApplicationVersion], new ApplicationVersionSerializer)
    k.register(classOf[InetSocketAddress], new InetSocketAddressSerializer)
  }
}

