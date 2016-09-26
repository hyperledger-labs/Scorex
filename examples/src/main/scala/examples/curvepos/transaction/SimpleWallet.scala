package examples.curvepos.transaction

import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PublicKey25519}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.util.{Success, Try}

import Constants25519.PrivKeyLength

class SimpleWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength))
  extends Wallet[PublicKey25519Proposition, SimpleTransaction, SimpleWallet] {
  override type S = PrivateKey25519
  override type PI = PublicKey25519
  override type NVCT = this.type


  private val secret: S = {
    val pair = Curve25519.createKeyPair(seed)
    PrivateKey25519(pair._1, pair._2)
  }

  override def secretByPublicImage(publicImage: PI): Option[S] = {
    if (publicImage.address == secret.publicImage.address) Some(secret) else None
  }

  override def generateNewSecret(): SimpleWallet = this

  override def secrets: Set[S] = Set(secret)

  override def rollback(to: VersionTag): Try[SimpleWallet] = Success(this)

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def scan(tx: SimpleTransaction): SimpleWallet = this

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = {
    Seq()
  }

  override def bulkScan(txs: Seq[SimpleTransaction]): SimpleWallet = ???

  override def companion: NodeViewComponentCompanion = ???
}
