package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

class PublicKey25519PropositionSerializer extends ScorexSerializer[PublicKey25519Proposition] {

  private val PortValueLength: Int = 4

  override def write(kryo: Kryo, output: Output, prop: PublicKey25519Proposition): Unit = {
    output.writeBytes(prop.pubKeyBytes)
  }

  override def read(kryo: Kryo, input: Input, c: Class[PublicKey25519Proposition]): PublicKey25519Proposition = {
    PublicKey25519Proposition(input.readBytes(Curve25519.KeyLength))
  }

}

