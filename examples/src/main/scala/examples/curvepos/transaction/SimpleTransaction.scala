package examples.curvepos.transaction

import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

sealed trait SimpleTransaction extends Transaction[PublicKey25519Proposition]

case class SimplePayment(sender: PublicKey25519Proposition,
                         recipient: PublicKey25519Proposition,
                         amount: Long,
                         fee: Long,
                         nonce: Long,
                         timestamp: Long)
  extends SimpleTransaction {

  override type M = SimplePayment

  override lazy val json: Json = Map(
    "sender" -> sender.toString.asJson,
    "recipient" -> recipient.toString.asJson,
    "amount" -> amount.asJson,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override def id: ModifierId = FastCryptographicHash(sender.address + nonce)

  override def equals(obj: Any): Boolean = obj match {
    case acc: SimplePayment => acc.id sameElements this.id
    case _ => false
  }

  override def hashCode(): Int = (BigInt(id) % Int.MaxValue).toInt
}
