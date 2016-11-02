package scorex.core.transaction.wallet

import scorex.core.NodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret

import scala.util.Try

case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: Array[Byte], createdAt: Long)

//todo: for Dmitry
object WalletBox {
  def parse[P <: Proposition, B <: Box[P]](bytes: Array[Byte]): Try[WalletBox[P, B]] = ???

  def bytes[P <: Proposition, B <: Box[P]](box: WalletBox[P, B]): Array[Byte] = ???
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