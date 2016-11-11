package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import com.google.common.primitives.Longs
import examples.curvepos.transaction.SimplePayment
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}

class SimplePaymentSerializer extends ScorexSerializer[SimplePayment] {
  override def write(kryo: Kryo, output: Output, m: SimplePayment): Unit = {
    kryo.writeObject(output, m.sender)
    kryo.writeObject(output, m.recipient)
    output.writeLong(m.amount)
    output.writeLong(m.fee)
    output.writeLong(m.nonce)
    output.writeLong(m.timestamp)

  }

  override def read(kryo: Kryo, input: Input, c: Class[SimplePayment]): SimplePayment = {
    val sender = kryo.readObject(input, classOf[PublicKey25519Proposition])
    val recipient = kryo.readObject(input, classOf[PublicKey25519Proposition])
    val amount = input.readLong()
    val fee = input.readLong()
    val nonce = input.readLong()
    val timestamp = input.readLong()
    SimplePayment(sender, recipient, amount, fee, nonce, timestamp)
  }
}
