package examples.curvepos.transaction

import io.circe.syntax._
import scorex.core.transaction.NodeViewModifier.ModifierId
import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.{NodeViewModifierCompanion, Transaction, TransactionChanges}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PublicKey25519


sealed trait SimpleTransaction
  extends Transaction[PublicKey25519Proposition]


/**
  * Transaction that sends fee to a miner
  */
case class FeeTransaction(boxId: Array[Byte], fee: Long, timestamp: Long)
  extends SimpleTransaction {

  def genesisChanges(): TransactionChanges[PublicKey25519Proposition] =
    TransactionChanges(Set(), Set(), fee)

  override def json: Json = Map("transaction" -> "Not implemented").asJson

  override val messageToSign: Array[Byte] = Longs.toByteArray(fee) ++ Longs.toByteArray(timestamp)

  override def companion: NodeViewModifierCompanion[FeeTransaction] = ???

  override def id: ModifierId = FastCryptographicHash(messageToSign)

  override type M = FeeTransaction
}


case class SimplePayment(sender: PublicKey25519, recipient: PublicKey25519, amount: Long, fee: Long, timestamp: Long)
  extends SimpleTransaction {

  override def json: Json = ???

  override lazy val messageToSign: Array[Byte] = id

  override type M = SimplePayment

  //todo: check statically or dynamically output size
  override lazy val id: ModifierId = FastCryptographicHash(
    sender.bytes ++
      recipient.bytes ++
      Longs.toByteArray(amount) ++
      Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp))

  override def companion: NodeViewModifierCompanion[SimplePayment] = ???
}