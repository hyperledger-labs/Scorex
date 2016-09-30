package scorex.core.transaction.wallet

import scorex.core.NodeViewComponent
import scorex.core.transaction.{NodeViewModifier, Transaction}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret
import scala.util.Try

case class WalletBox[P <: Proposition](box: Box[P], transactionId: Array[Byte], createdAt: Int, destroyedAt: Option[Int])

case class WalletTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX, createdAt: Int)

trait Vault[P <: Proposition, TX <: Transaction[P], V <: Vault[P, TX, V]] extends NodeViewComponent {
  type VersionTag = NodeViewModifier.ModifierId

  //todo: or Try[Wallet[P, TX]] ?
  def scan(tx: TX, offchain: Boolean): V

  def bulkScan(txs: Seq[TX], offchain: Boolean): V

  def rollback(to: VersionTag): Try[V]
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

  def historyBoxes: Seq[WalletBox[P]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}


