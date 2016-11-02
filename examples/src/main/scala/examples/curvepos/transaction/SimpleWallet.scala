package examples.curvepos.transaction

import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.util.Try
import Constants25519.PrivKeyLength
import scorex.core.transaction.Transaction

import Transaction.TransactionId

case class SimpleWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                        chainTransactions: Map[TransactionId, SimpleTransaction] = Map(),
                        offchainTransactions: Map[TransactionId, SimpleTransaction] = Map(),
                        currentBalance: Long = 0)
  extends Wallet[PublicKey25519Proposition, SimpleTransaction, SimpleBlock, SimpleWallet] {
  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  override type NVCT = SimpleWallet

  //it's being recreated from seed on each wallet update, not efficient at all
  private val secret: S = {
    val pair = Curve25519.createKeyPair(seed)
    PrivateKey25519(pair._1, pair._2)
  }

  private val pubKeyBytes = secret.publicKeyBytes

  override def secretByPublicImage(publicImage: PI): Option[S] = {
    if (publicImage.address == secret.publicImage.address) Some(secret) else None
  }

  override def generateNewSecret(): SimpleWallet = throw new Error("Only one secret is supported")

  override def secrets: Set[S] = Set(secret)

  override def rollback(to: VersionTag): Try[SimpleWallet] = ???

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleTransaction]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = Seq()

  override def scanOffchain(tx: SimpleTransaction): SimpleWallet = tx match {
    case sp: SimplePayment =>
      if ((sp.sender.bytes sameElements pubKeyBytes) || (sp.recipient.bytes sameElements pubKeyBytes)) {
        SimpleWallet(seed, chainTransactions, offchainTransactions + (sp.id -> sp), currentBalance)
      } else this
  }

  override def scanOffchain(txs: Seq[SimpleTransaction]): SimpleWallet =
    txs.foldLeft(this) { case (wallet, tx) => wallet.scanOffchain(tx) }

  override def scanPersistent(modifier: SimpleBlock): SimpleWallet = {
    modifier.transactions.map(_.foldLeft(this) { case (w, tx) =>
      tx match {
        case sp: SimplePayment =>
          if ((sp.sender.bytes sameElements pubKeyBytes) || (sp.recipient.bytes sameElements pubKeyBytes)) {
            val ct = w.chainTransactions + (sp.id -> sp)
            val oct = w.offchainTransactions - sp.id
            val cb = if (sp.recipient.bytes sameElements pubKeyBytes) {
              w.currentBalance + sp.amount
            } else w.currentBalance - sp.amount
            SimpleWallet(seed, ct, oct, cb)
          } else w
      }
    }).getOrElse(this)
  }

  override def companion: NodeViewComponentCompanion = ???
}