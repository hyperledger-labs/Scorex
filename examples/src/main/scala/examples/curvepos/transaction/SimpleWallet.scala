package examples.curvepos.transaction

import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PublicKey25519, PrivateKey25519}
import scorex.core.transaction.wallet.{WalletBox, WalletTransaction, Wallet}
import scorex.crypto.signatures.Curve25519

import scala.util.{Success, Try}

class SimpleWallet(seed: Array[Byte]) extends Wallet[PublicKey25519Proposition, FeeTransaction, SimpleWallet] {
  override type S = PrivateKey25519
  override type PI = PublicKey25519
  override type NVCT = this.type


  private val secret:S = {
    val pair = Curve25519.createKeyPair(seed)
    PrivateKey25519(pair._1, pair._2)
  }

  override def secretByPublicImage(publicImage: PI): Option[S] = {
    if(publicImage.address == secret.publicImage.address) Some(secret) else None
  }

  override def generateNewSecret(): SimpleWallet = this

  override def secrets: Set[S] = Set(secret)

  override def rollback(to: VersionTag): Try[SimpleWallet] = Success(this)

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def scan(tx: FeeTransaction): SimpleWallet = this

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, FeeTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = {
    Seq()
  }

  override def bulkScan(txs: Seq[FeeTransaction]): SimpleWallet = ???

  override def companion: NodeViewComponentCompanion = ???

}
