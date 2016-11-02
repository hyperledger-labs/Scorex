package scorex.core.transaction.wallet

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.NodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret

import scala.util.Try

case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: Array[Byte], createdAt: Long)

object WalletBox {
  def parse[P <: Proposition, B <: Box[P]](bytes: Array[Byte]): Try[WalletBox[P, B]] = Try {
    val txId = bytes.slice(0, NodeViewModifier.ModifierIdSize)
    val createdAt = Longs.fromByteArray(
      bytes.slice(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize + 8))
    val boxB = bytes.slice(NodeViewModifier.ModifierIdSize + 8, bytes.length)
    val box: B = ???
    WalletBox(box, txId, createdAt)
  }

  def bytes[P <: Proposition, B <: Box[P]](box: WalletBox[P, B]): Array[Byte] = {
    Bytes.concat(box.transactionId, Longs.toByteArray(box.createdAt), box.box.bytes)
  }
}

case class WalletTransaction[P <: Proposition, TX <: Transaction[P]](proposition: P,
                                                                     tx: TX,
                                                                     blockId: Option[NodeViewModifier.ModifierId],
                                                                     createdAt: Long)

//todo: for Dmitry
object WalletTransaction {
  def parse[P <: Proposition, TX <: Transaction[P]](bytes: Array[Byte]): Try[WalletTransaction[P, TX]] = ???

  def bytes[P <: Proposition, TX <: Transaction[P]](box: WalletTransaction[P, TX]): Array[Byte] = ???
}


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait Wallet[P <: Proposition, TX <: Transaction[P], W <: Wallet[P, TX, W]]
  extends Vault[P, TX, W] {

  type S <: Secret
  type PI <: ProofOfKnowledgeProposition[S]

  def generateNewSecret(): W

  def historyTransactions: Seq[WalletTransaction[P, TX]]

  def boxes(): Seq[WalletBox[P, _ <: Box[P]]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}