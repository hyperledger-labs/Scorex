package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.NodeViewModifier
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.WalletBox

import scala.reflect.ClassTag

class WalletBoxSerializer[P <: Proposition, B <: Box[P] : ClassTag] extends ScorexSerializer[WalletBox[P, B]] {

  override def write(kryo: Kryo, output: Output, data: WalletBox[P, B]): Unit = {
    output.writeBytes(data.transactionId)
    output.writeLong(data.createdAt)
    kryo.writeObject(output, data.box)
  }

  override def read(kryo: Kryo, input: Input, c: Class[WalletBox[P, B]]): WalletBox[P, B] = {
    val txId = input.readBytes(NodeViewModifier.ModifierIdSize)
    val createdAt = input.readLong()
    val box: B = kryo.readObject(input, getClassFromClassTag[B])
    WalletBox[P, B](box, txId, createdAt)
  }
}
