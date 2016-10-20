package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.twitter.chill.IKryoRegistrar
import examples.curvepos.transaction.{SimpleBlock, SimplePayment}


class CurveposRegistrar extends IKryoRegistrar {
  override def apply(k: Kryo): Unit = {
    k.setRegistrationRequired(true)
    k.setReferences(false)
    k.register(classOf[SimplePayment], new SimplePaymentSerializer)
    k.register(classOf[SimpleBlock], new SimpleBlockSerializer)
  }
}

