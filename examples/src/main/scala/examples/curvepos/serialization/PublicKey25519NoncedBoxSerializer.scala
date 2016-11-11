package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class PublicKey25519NoncedBoxSerializer extends ScorexSerializer[PublicKey25519NoncedBox] {
  override def write(kryo: Kryo, output: Output, m: PublicKey25519NoncedBox): Unit = {
    kryo.writeObject(output, m.proposition)
    output.writeLong(m.nonce)
    output.writeLong(m.value)
  }

  override def read(kryo: Kryo, input: Input, c: Class[PublicKey25519NoncedBox]): PublicKey25519NoncedBox = {
    val prop = kryo.readObject(input, classOf[PublicKey25519Proposition])
    val nonce = input.readLong()
    val value = input.readLong()
    PublicKey25519NoncedBox(prop, nonce, value)
  }
}
