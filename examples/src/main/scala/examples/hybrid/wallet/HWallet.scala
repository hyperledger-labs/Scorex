package examples.hybrid.wallet

import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.Transaction._
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.utils.Random

import scala.util.Try



case class HWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                        chainTransactions: Map[TransactionId, SimpleBoxTransaction] = Map(),
                        offchainTransactions: Map[TransactionId, SimpleBoxTransaction] = Map(),
                        currentBalance: Long = 0)
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, HWallet]  {
  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  override def generateNewSecret(): HWallet = ???

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleBoxTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] = ???

  //todo: protection?
  override def secrets: Set[PrivateKey25519] = ???

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] = ???

  override def scan(tx: SimpleBoxTransaction, offchain: Boolean): HWallet = ???

  override def bulkScan(txs: Seq[SimpleBoxTransaction], offchain: Boolean): HWallet = ???

  override def rollback(to: VersionTag): Try[HWallet] = ???

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}
