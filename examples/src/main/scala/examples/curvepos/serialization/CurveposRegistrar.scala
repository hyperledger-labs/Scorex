package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.twitter.chill.IKryoRegistrar
import examples.curvepos.SimpleSyncInfo
import examples.curvepos.transaction.{PublicKey25519NoncedBox, SimpleBlock, SimplePayment}
import scorex.core.serialization.WalletBoxSerializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.WalletBox


class CurveposRegistrar extends IKryoRegistrar {
  override def apply(k: Kryo): Unit = {
    k.setRegistrationRequired(true)
    k.setReferences(false)
    k.register(classOf[SimplePayment], new SimplePaymentSerializer)
    k.register(classOf[SimpleBlock], new SimpleBlockSerializer)
    k.register(classOf[SimpleSyncInfo], new SimpleSyncInfoSerializer)
    k.register(classOf[PublicKey25519NoncedBox], new PublicKey25519NoncedBoxSerializer)
    k.register(classOf[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]],
      new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox])
  }
}

