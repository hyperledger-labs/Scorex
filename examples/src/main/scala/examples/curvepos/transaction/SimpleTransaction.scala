package examples.curvepos.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.{BoxTransaction, Transaction}
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait SimpleTransaction extends BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox]{
  val fee: Long

  val timestamp: Long
}

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

  override lazy val messageToSign: Array[Byte] = id

  override lazy val serializer = SimplePaymentCompanion

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = ???
  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = ???
}

object SimplePaymentCompanion extends Serializer[SimplePayment] {
  val TransactionLength: Int = 2 * Constants25519.PubKeyLength + 32

  override def toBytes(m: SimplePayment): Array[Byte] = {
    m.sender.bytes ++
      m.recipient.bytes ++
      Longs.toByteArray(m.amount) ++
      Longs.toByteArray(m.fee) ++
      Longs.toByteArray(m.nonce) ++
      Longs.toByteArray(m.timestamp)
  }.ensuring(_.length == TransactionLength)

  override def parseBytes(bytes: Array[Byte]): Try[SimplePayment] = Try {
    val sender = PublicKey25519Proposition(bytes.slice(0, Constants25519.PubKeyLength))
    val recipient = PublicKey25519Proposition(bytes.slice(Constants25519.PubKeyLength, 2 * Constants25519.PubKeyLength))
    val s = 2 * Constants25519.PubKeyLength
    val amount = Longs.fromByteArray(bytes.slice(s, s + 8))
    val fee = Longs.fromByteArray(bytes.slice(s + 8, s + 16))
    val nonce = Longs.fromByteArray(bytes.slice(s + 16, s + 24))
    val timestamp = Longs.fromByteArray(bytes.slice(s + 24, s + 32))
    SimplePayment(sender, recipient, amount, fee, nonce, timestamp)
  }
}