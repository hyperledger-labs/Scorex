package examples.curvepos.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.ModifierId
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
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

  override lazy val id: ModifierId = FastCryptographicHash(bytes)

  override val companion: NodeViewModifierCompanion[SimplePayment] = SimpleTransaction
}


object SimpleTransaction extends NodeViewModifierCompanion[SimplePayment] {
  val TransactionLength: Int = 2 * Constants25519.PubKeyLength + 32

  override def bytes(m: SimplePayment): Array[Byte] = {
    m.sender.bytes ++
      m.recipient.bytes ++
      Longs.toByteArray(m.amount) ++
      Longs.toByteArray(m.fee) ++
      Longs.toByteArray(m.nonce) ++
      Longs.toByteArray(m.timestamp)
  }.ensuring(_.length == TransactionLength)

  override def parse(bytes: Array[Byte]): Try[SimplePayment] = Try {
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