package examples.curvepos.transaction

import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

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
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "recipient" -> Base58.encode(recipient.pubKeyBytes).asJson,
    "amount" -> amount.asJson,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  //TODO ???
  override def id: ModifierId = sender.pubKeyBytes

  override def equals(obj: Any): Boolean = obj match {
    case acc: SimplePayment => acc.id sameElements this.id
    case _ => false
  }

  override def hashCode(): Int = (BigInt(id) % Int.MaxValue).toInt
}
