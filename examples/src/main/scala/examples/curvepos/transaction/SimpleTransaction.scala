package examples.curvepos.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.{NodeViewModifierCompanion, Transaction}
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait SimpleTransaction extends Transaction[PublicKey25519Proposition]


case class SimplePayment(sender: PublicKey25519Proposition,
                         recipient: PublicKey25519Proposition,
                         amount: Long,
                         fee: Long,
                         nonce: Long,
                         timestamp: Long)
  extends SimpleTransaction {

  override def json: Json = Map(
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "recipient" -> Base58.encode(recipient.pubKeyBytes).asJson,
    "amount" -> amount.asJson,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = id

  override type M = SimplePayment

  //todo: check statically or dynamically output size
  override lazy val id: ModifierId = FastCryptographicHash(
    sender.bytes ++
      recipient.bytes ++
      Longs.toByteArray(amount) ++
      Longs.toByteArray(fee) ++
      Longs.toByteArray(nonce) ++
      Longs.toByteArray(timestamp))

  override val companion: NodeViewModifierCompanion[SimplePayment] = SimpleTransaction
}


object SimpleTransaction extends NodeViewModifierCompanion[SimplePayment] {
  val TransactionLength: Int = ???

  override def bytes(modifier: SimplePayment): Array[Byte] = ???

  override def parse(bytes: Array[Byte]): Try[SimplePayment] = ???
}
