package examples.curvepos.transaction

import io.circe.syntax._
import scorex.core.transaction.NodeViewModifier.ModifierId
import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.{NodeViewModifierCompanion, Transaction, TransactionChanges}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState

import scala.util.Try

sealed trait SimplestTransaction
  extends Transaction[PublicKey25519Proposition, SimplestTransaction]



/**
  * Transaction that sends fee to a miner
  */
case class FeeTransaction(boxId: Array[Byte], fee: Long, timestamp: Long)
  extends SimplestTransaction {

  def genesisChanges(): TransactionChanges[PublicKey25519Proposition] =
    TransactionChanges(Set(), Set(), fee)

  override def changes(state: MinimalState[PublicKey25519Proposition, SimplestTransaction, _, _]): Try[TransactionChanges[PublicKey25519Proposition]] = Try {
    //TODO saInstanceOf
    if (state.asInstanceOf[MinimalStateImpl].isEmpty) genesisChanges()
    else {
      state.closedBox(boxId) match {
        case Some(oldSender: PublicKey25519NoncedBox) =>
          val newSender = oldSender.copy(value = oldSender.value - fee, nonce = oldSender.nonce + 1)
          require(newSender.value >= 0)

          TransactionChanges[PublicKey25519Proposition](Set(oldSender), Set(newSender), fee)
        case _ => throw new Exception("Wrong kind of box")
      }
    }

  }

  override def validate(state: MinimalState[PublicKey25519Proposition, SimplestTransaction, _, _]): Try[Unit] = Try {
    state.closedBox(boxId).get
  }

  override def json: Json = Map("transaction" -> "Not implemented").asJson

  override val messageToSign: Array[Byte] = Longs.toByteArray(fee) ++ Longs.toByteArray(timestamp)

  override def companion: NodeViewModifierCompanion[FeeTransaction.this.type] = ???

  override def id(): ModifierId = FastCryptographicHash(messageToSign)

  override type M = this.type
}
