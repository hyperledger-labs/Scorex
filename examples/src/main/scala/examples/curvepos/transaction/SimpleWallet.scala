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
  extends Wallet[PublicKey25519Proposition, SimpleTransaction, SimpleWallet] {
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

  /**
    * Only one secret is supported
    *
    * @return
    */
  override def generateNewSecret(): SimpleWallet = SimpleWallet(Random.randomBytes(PrivKeyLength))

  override def generateNewSecret(seed: Array[Byte]): SimpleWallet = SimpleWallet(seed)

  override def secrets: Set[S] = Set(secret)

  override def rollback(to: VersionTag): Try[SimpleWallet] = ???

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def scan(tx: SimpleTransaction, offchain: Boolean): SimpleWallet = tx match {
    case sp: SimplePayment =>
      if ((sp.sender.bytes sameElements pubKeyBytes) || (sp.recipient.bytes sameElements pubKeyBytes)) {
        if (offchain) {
          SimpleWallet(seed, chainTransactions, offchainTransactions + (sp.id -> sp), currentBalance)
        } else {
          val ct = chainTransactions + (sp.id -> sp)
          val oct = offchainTransactions - sp.id
          val cb = if (sp.recipient.bytes sameElements pubKeyBytes) {
            currentBalance + sp.amount
          } else currentBalance - sp.amount
          SimpleWallet(seed, ct, oct, cb)
        }
      } else this
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = {
    Seq()
  }

  override def bulkScan(txs: Seq[SimpleTransaction], offchain: Boolean): SimpleWallet =
    txs.foldLeft(this) { case(wallet, tx) => wallet.scan(tx, offchain)}

  override def companion: NodeViewComponentCompanion = ???
}
