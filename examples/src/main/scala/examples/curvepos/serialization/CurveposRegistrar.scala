package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.twitter.chill.IKryoRegistrar
import examples.curvepos.SimpleSyncInfo
import examples.curvepos.transaction.{PublicKey25519NoncedBox, SimpleBlock, SimplePayment}


class CurveposRegistrar extends IKryoRegistrar {
  override def apply(k: Kryo): Unit = {
    k.setRegistrationRequired(true)
    k.setReferences(false)
    k.register(classOf[SimplePayment], new SimplePaymentSerializer)
    k.register(classOf[SimpleBlock], new SimpleBlockSerializer)
    k.register(classOf[SimpleSyncInfo], new SimpleSyncInfoSerializer)
    k.register(classOf[PublicKey25519NoncedBox], new PublicKey25519NoncedBoxSerializer)
  }
}

